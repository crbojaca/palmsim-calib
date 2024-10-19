#!/usr/bin/env python
''' '''

import yaml
import os
import sys

from datetime import datetime, timedelta
import calendar

import numpy as np
import pandas as pd

from .components.helpers import add_dumps

from .components.fronds import Fronds
from .components.trunk import Trunk
from .components.roots import Roots
from .components.generative import Cohorts
from .components.assimilates import Assimilates
from .components.soil import Soil
from .components.weather import Weather
from .components.generative import Indeterminate, Male, Female
from .components.generative import Stalk, MesocarpFibers, MesocarpOil, Kernels

MODULE_FILEPATH = sys.modules[__name__].__file__
MODULE_DIR = os.path.dirname(MODULE_FILEPATH)


@add_dumps
class PalmField():
    ''' The main PalmSim class, models a field of oil palms.

    Here a field of palms is modelled by the underlying
    components (sub-models):

            - fronds
            - trunks
            - roots
            - assimilates
            - cohorts
            - weather
            - soil

    The model of the palm field follows from
    these sub-models.

    Given a palm field "pf"

        pf = PalmField()

    we have e.g. access to the fronds sub-model via

        pf.fronds

    For a list of all instance variables

        pf.instance_variables

    See the "getting started" below.

    Notes
    -----
    All the interesting stuff --- the actual model implementation ---
    can be found in the sub-models; this class is a so-called
    "container class":

    This role of this class is to link and manage the sub-models.
    Note, we at any time have access to the sub-models.
    For means of development we can even run the sub-models
    by themselves, standalone.

    In our opinion this is a good design of the implementation.

    Getting Started
    ---------------

    Given a palm field "pf"

        pf = PalmField()

    we have e.g. access to the fronds sub-model via

        pf.fronds

    the weather sub-model via

        pf.weather

    For a list of all the components that can be accessed
    in such a way simply access

        pf.components

    Running

        pf

    by itself displays a neat list of all the model
    variables. Similarly for a component e.g.

        pf.fronds

    For a list of all instance variables

        pf.instance_variables

    To update the palm field one time-step (one month)

        pf.update()


    To retrieve all the exposed component variables

        pf.to_dict()

    To instantiate a palm field at a certain year and time
    e.g. the 1st of January 2017

        pf = PalmField(year_of_planting = 2017, month_of_planting = 0)

    note we can aswell interchange the order of arguments since
    we are using key-word arguments and we count the months zero-based

        pf = PalmField(month_of_planting = 0, year_of_planting = 2017)

    '''

    units = yaml.load("""
        age                  = 'month'
        dt                   = 'month'
        harvest              = 'tonne_DM/ha/month'
        harvest_yearly       = 'tonne_DM/ha/year'
        mass_fronds          = 'tonne_DM/ha'
        mass_roots           = 'tonne_DM/ha'
        mass_trunk           = 'tonne_DM/ha'
        month                = 'month'
        planting_density     = '1/ha'
        assim_synth_total    = 'ton_CH2O/ha/month'
        trunk_mass_per_palm  = 'kg_DM/palm'
        roots_mass_per_palm  = 'kg_DM/palm'
        fronds_mass_per_palm = 'kg_DM/palm'
        mass_generative      = 'tonne_DM/ha'
        mass_vegetative      = 'tonne_DM/ha'
        yield_DM             = 'tonne_DM/ha/month'
    """,
                      Loader=yaml.SafeLoader)

    _prefix = 'palm'

    parameters = {}

    # A default planting_density of 143 is used.
    def __init__(self,
                 verbose=True,
                 settings=None,
                 year_of_planting=2001,
                 month_of_planting=6,
                 day_of_planting=15,
                 planting_density=143,
                 latitude=0,
                 soil_texture_class='loamy sand',
                 soil_depth=1,
                 dt=10):

        # simulation run-time is kept by instances of this class

        self.year_of_planting = year_of_planting
        self.month_of_planting = month_of_planting
        self.day_of_planting = day_of_planting

        self.planting_density = planting_density

        self.dt = dt

        self.latitude = latitude

        self.time = datetime(year_of_planting, month_of_planting,
                             day_of_planting)

        # assignment by value
        self.time_of_planting = self.time

        # Link the sub-models
        self.weather = Weather(self)
        self.soil = Soil(self,
                         soil_texture_class=soil_texture_class,
                         soil_depth=soil_depth)
        self.fronds = Fronds(self)
        self.roots = Roots(self)
        self.trunk = Trunk(self)
        self.generative = Cohorts(self)
        self.indeterminate = Indeterminate(self.generative)
        self.female = Female(self.generative)
        self.male = Male(self.generative)

        self.assimilates = Assimilates(self)

        self.components = [
            self.fronds,
            self.roots,
            self.trunk,
            self.generative,
            self.assimilates,
            self.weather,
            self.soil,
        ]

        self._units = {}

    @property
    def DOY(self):
        ''' Day of the year (1--366). '''
        return self.time.now().timetuple().tm_yday

    @property
    def DAP(self):
        """ Days after planting (days). """
        time_passed = self.time - self.time_of_planting
        return time_passed.days

    @property
    def YAP(self):
        """ Years after planting (years). """
        return self.year - self.year_of_planting

    @property
    def MAP(self):
        """ Months after planting (months). """
        month = self.month - self.month_of_planting
        YAP = self.YAP

        return 12 * YAP + month

    @property
    def _days_in_month(self):
        """ Number of days in the month. """

        year = self.year
        month = self.month

        return calendar.monthrange(year, month)[1]

    @property
    def _days_in_year(self):
        """ Number of days in year. """

        year = self.year

        leap_year = calendar.isleap(year)

        if leap_year:
            return 366
        else:
            return 365

    @property
    def year(self):
        ''' Year. '''
        return self.time.year

    @property
    def month(self):
        ''' Month of the year (1--12). '''
        return self.time.month

    @property
    def day(self):
        ''' Day of the month (1--31). '''
        return self.time.day

    @property
    def date(self):
        ''' The date (yyyy-mm-dd). '''
        return '{:}-{:}-{:}'.format(self.year, self.month, self.day)

    @property
    def date_tuple(self):
        return (self.year, self.month, self.day)

    def update(self):
        ''' Update by dt days. '''

        dt = self.dt

        assert isinstance(dt, int)
        assert dt <= 31
        assert dt >= 1

        self._update(dt=dt)

        return self

    def _update(self, dt):
        ''' Update by dt days. '''

        _dt = timedelta(days=dt)

        self.time += _dt

        self.weather.update()
        self.assimilates.update()

        self.fronds.update(dt=dt)
        self.trunk.update(dt=dt)
        self.roots.update(dt=dt)

        self.generative.update(dt=dt)

        self.soil.update(dt=dt)

    #########################
    # Mass: alternative units
    #########################

    @property
    def mass_total(self):
        ''' The mass of all the palms together (t DM/ha). '''
        return self.mass_vegetative + self.mass_generative

    @property
    def mass_vegetative(self):
        ''' The mass of all the vegetative parts together (t DM/ha). '''
        return self.fronds.mass + self.trunk.mass + self.roots.mass

    @property
    def mass_generative(self):
        ''' The mass of all the generative Cohorts (t DM/ha). '''
        return self.generative.mass

    @property
    def trunk_mass_per_palm(self):
        ''' The mean mass of the palm trunks (kg). '''
        return 1000 * self.trunk.mass / self.planting_density

    @property
    def roots_mass_per_palm(self):
        ''' The mean mass of the palm roots (kg). '''
        return 1000 * self.roots.mass / self.planting_density

    @property
    def fronds_mass_per_palm(self):
        ''' The mean mass of the palm fronds (kg). '''
        return 1000 * self.fronds.mass / self.planting_density

    def run(self, duration=30 * 365):

        dt = self.dt

        nsteps = duration // dt

        res = {}

        for i in range(nsteps):

            self._update(dt=dt)

            res[i] = self.to_dict()

        df = pd.DataFrame(res).T

        df = df.set_index(pd.to_datetime(df['date']))

        df = df.apply(pd.to_numeric, errors='ignore')

        df['FFB_production (kg/ha/yr)'] = df[
            'generative_FFB_production (t/ha/yr)']

        return df

    ########
    # output
    ########

    @property
    def instance_variables(self):
        ''' The list of instance variables. '''
        return [attr for attr in dir(self) if not attr.startswith('_')]

    def to_dict(self, hide_attr=True):
        ''' Output the variable values to a dictionary. '''

        d = self._to_dict()

        for component in self.components:

            d_ = component.to_dict(prefixed=True)

            d.update(d_)

        return d

    def _to_dict(self):
        ''' Returns a dictionary of direct float-like instance variables. '''

        attributes = self.instance_variables
        units = self.units

        d = {}

        for key in attributes:

            try:
                value = getattr(self, key)
            except:
                print(sys.exc_info[0])
                pass

            if isinstance(value, (float, int, str)):
                d[key] = value
            else:
                pass

        return d

    @property
    def units(self):
        return self._units

    def get_units(self):
        ''' Get the associated units. '''
        return {}


def my_replace(s):
    ''' Replace some sub-strings in a string.

        '_' -> ' '
        'tonne_DM' -> 't'

        >>> my_replace('alfa_beta')
        'alfa beta'
    '''
    s = s.replace('tonne_CH2O', 't')
    s = s.replace('tonne_DM', 't')
    s = s.replace('_', ' ')
    return s

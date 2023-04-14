## Collection Methods
method    | start date | end date | notes 
----------|------------|----------|-------
Jaime's   | 2015       | 2017-03  | 
Natalia's | 2017-06    | 2020-12  | Natalia did some crossover with Jaime to provide comparison

* Natalia's Method: [Zooplankton Methodology, Collection & Identification - a field manual](https://drs.nio.org/drs/handle/2264/95)

## Raw Data Files
Metadata file structure from the cruises has not changed significantly.

filename                                    | explanation
--------------------------------------------|----------------
WSMasterSampleLog.xlsx                      | CTD water samples for all stations that can be used for MoF. Updated versions provided by Ian.
cruise_logsheets/{cruise_id}                | details for MoF as needed.

### Jaime's
filename                                    | explanation
--------------------------------------------|----------------
compiled_zoo_taxonomy_Jaimie_31JAN2018.xlsx | Species counts for all samples compliled into one file by Jaimie 2018-01-31. Has header information & species counts. 
zooplankton_consolidated.xlsx               | Details of sampling methods for each sampling event. Data from this is used to calculate "density" values in the compiled_zoo_taxonomy file. TODO: rename from `zoo_consolidated` to `zoo_cruise_metadata`.
sp.csv                                      | manually created mapping from Jaime's shorthand `classificaion` to a `sciName` that WoRMS is able to translate to a `aphiaID`.

### Natalia's
Samples separated into directories by mesh size.

filename                                                                 |
-------------------------------------------------------------------------|---
{ship_id}{year}{julian_day_of_cruise_start}_{station_number}_{mesh_size} | Taxa counts for each aliquot. Includes header with event collection data. Some have misspellings in header info.

## 

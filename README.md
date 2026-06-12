# ACTRIS/EARLINET Level-3-Aerosol-Profiling-Climatology

# Project Documentation

## Overview
This project involves the processing of atmospheric lidar data using a modular set of R scripts. The scripts are organized to handle different levels of data processing, including Level 2 and Level 3 data. These include profile-based, integrated, and layer-resolved statistics, as well as the generation of standardized NetCDF output files for climatological and annual summaries.

## Structure
The project consists of the following main components:

- **Main Script**: The main execution point for the data processing pipeline.
- **Level 2 Data Processing**: Scripts for processing Level 2 data from NetCDF files.
- **Level 3 Data Processing**: Scripts for processing Level 3 data, including profile data, integrated data, and layer-specific data.
- **System Metadata**: Script for extracting system metadata from NetCDF files.
- **Auxiliary Files**: Log files and CSV files used for data processing.
- **NetCDF Output Generation**: Scripts for generating standardized Level 3 NetCDF files (monthly, seasonal, annual) for both integrated and layer-resolved data, following ACTRIS conventions.

## Dependencies
The project requires the following R packages (notably `ncdf4` for NetCDF file I/O):

- `isotone`
- `ncdf4`
- `radiant.data`
- `dplyr`
- `tidyr`

## Execution
To execute the project, follow these steps:

1. Ensure all required R packages are installed.
2. Place all scripts and auxiliary files in the same directory.
3. Run the main script (`Main.R`) to start the data processing pipeline. This script sequentially sources and executes all other scripts required for Level 2 and Level 3 processing.
4. The output files are saved in structured directories under `Level3/Profiles/`, `Level3/Integrated/` and `Level3/Layers/`, organized by station and data type.

## Output
The processing pipeline produces the following types of NetCDF files:

- **Profile Data Files**:
  - Monthly, seasonal, and annual vertical profiles for extinction coefficient, backscatter coefficient, lidar ratio, particle depolarization ratio, AOD and integrated backscatter profiles.
  - Stored in: `Level3/Profiles/<station>/`
  
- **Integrated Data Files**:
  - Monthly, seasonal, and annual statistics for AOD, backscatter, lidar ratio, particle depolarization, and PBL height.
  - Stored in: `Level3/Integrated/<station>/`

- **Layer-Resolved Data Files**:
  - Monthly, seasonal, and annual histograms of layer properties (e.g., base/top altitude, extinction, lidar ratio).
  - Stored in: `Level3/Layers/<station>/`

Each file follows ACTRIS formatting conventions and includes metadata such as time bounds, wavelength, statistical descriptors (mean, median, standard deviation, etc.), and source file references.


## Scripts Documentation
## Main.R

### Purpose
This script orchestrates the execution of various R scripts and functions for processing atmospheric lidar data. It handles data loading, transformation, merging, and the generation of NetCDF files for different wavelengths and data types (profiles, integrated values, layers).

### Key Variables and Functions
Several helper functions are defined to:

- Count and filter values (len_0, cnt, cnt_int, unq)
- Handle seasonal formatting (sson1, sson2, seas, seas1)
- Compute weighted statistics (w_median, w_sd, etc.)
- Handle NetCDF variables safely (get_nc_var)
- Merge and filter datasets (filter_and_merge, etc.)
- Handle leap years and February days
- Calculate angular ratios and errors (ang, ang_err)

### Logical Flow
The script performs the following steps:

- Initialization:
  - Defines the release period (e.g., 2000–2021).
  - Loads external scripts for Level 2 and Level 3 data processing.
  
- Data Preparation:
  - Reads station metadata from station.csv.
  - Filters out unavailable stations.
  - Extracts station-specific information (location, altitude, PI, etc.).
  
- System Metadata Handling:
  - Extracts the last non-NA value from system metadata.
  - Assigns system descriptions based on station codes.
  
- Profile Data Processing:
  - Loads and processes profile data for wavelengths: 355, 532, 1064 nm.
  - Sets flags based on data availability.
  
- Integrated Data Processing:
  - Loads integrated data from Level 2 sources.
  - Processes and reorganizes data by wavelength.
  - Sets flags for each wavelength.
  
- Layer Data Processing:
  - Loads and processes layer data.
  - Applies scaling factors to extinction, backscatter, and other variables.
  - Computes quantile-based intervals for visualization or classification.
  
- Finalization:
  - Executes additional scripts for saving processed data.
  - Handles errors and ensures all file connections are closed.
  
- Error Handling:
  - The script uses `tryCatch` to catch and log errors.
  - Closing all open connections in the `finally` block via `closeAllConnections()`.


## Lev2database.R

### Purpose
This script constructs a metadata table (lev2db) for Level 2 lidar data files located in the ./New directory. It extracts file attributes, checks for matching file pairs, verifies the presence of specific NetCDF variables, and filters the dataset based on external logs and wavelength criteria.

### Input Files
- Lidar data files: Located in ./New, organized by station.
- Log files:
  - Climatol2.log
  - Calipso2.log

### Output
A filtered and enriched data frame lev2db containing metadata and flags for each lidar file.

### Logical Flow
The logical flow of the script is as follows:

- File Listing and Metadata Extraction:
  - Lists all files in ./New recursively.
  - Initializes a data frame lev2db with 10 columns to store metadata: File_name, Station, Type, Wavelength, Year, Month, Day, Hour, Minutes, VarBool
  - Extracts metadata from file names using substr.

- External Log Filtering:
  - Reads and trims Climatol2.log and Calipso2.log to exclude headers and footers.
  - Combines both logs into a unified list of valid file names (all_files).
  
- File Pair Matching:
  - For each file of type "e", checks if a corresponding "b" file exists with matching metadata.
  - Sets VarBool to 1 if a match is found, otherwise 0.
  
- Particle Depolarization Flags:
  - Initializes three new columns: PartDepFlag, PartDepFlag2, and PdF.
  - Opens each NetCDF file and checks for the presence of the variable particledepolarization.
    - If present, sets PartDepFlag to 1.
  - If the file is of type "e" and has PartDepFlag == 1, checks again for a matching "b" file.
    - If no match is found, sets PdF to 1.
  
- Final Filtering:
  - Keeps only files listed in all_files.
  - Removes intermediate flag columns (PartDepFlag, PartDepFlag2).
  - Filters the dataset to include:
    - Only files from station "laq" with wavelength "0351", or
    - Files from other stations with wavelengths "0355", "0532", or "1064".
    
### Notes
- The logic assumes a strict file naming convention for metadata extraction.
- The script is sensitive to NetCDF structure and may fail if expected variables are missing.


## Lev2database_layers.R

### Purpose
This script processes NetCDF files containing atmospheric layer data for each station. It extracts temporal and geometric information (base and top altitudes of layers) and merges it with the existing Level 2 metadata (lev2db) to produce a comprehensive dataset (lev2db_layers).

### Input Files
- NetCDF files: Located in subdirectories of ./Layers/, one per station.
- lev2db object: Must be loaded beforehand (commented out in the script).

### Output
- lev2db_layers: A merged data frame containing both file metadata and layer altitude information.
- layers_base_top: A simplified version containing only layer altitude data.

### Logical Flow
The logical flow of the script is as follows:

- Initialization:
  - Defines the path to the layer files directory (./Layers/).
  - Identifies stations that have both metadata in lev2db and corresponding layer files.
  
- Layer Data Extraction:
  - For each station:
    - Opens the first NetCDF file.
    -Extracts:
      - input_file: used to parse date and time.
      - geometrical_properties: a 3D array containing layer base and top altitudes.
    - For each time step:
      - Extracts date and time from input_file.
      - Extracts base (gp[2, , ]) and top (gp[4, , ]) altitudes for each layer.
      - Stores all values in a temporary data frame m0.

- Aggregation:
  - Appends each station’s data to a cumulative data frame layers_files.
  
- Cleaning and Transformation:
  - Renames columns for clarity.
  - Replaces invalid values (> 9e36) with NA.
  - Converts altitudes from kilometers to meters by multiplying by 1000.
  - Removes rows with missing top layer values.
  
- Merging with Metadata:
  - Merges layers_files with lev2db on station and timestamp fields to create lev2db_layers.
  
### Notes
- The script assumes a consistent structure in the NetCDF files.


## system.R  

### Purpose
This script extracts the "system" attribute from NetCDF metadata for each station and year in the Level 2 dataset. The result is a matrix (syst_df) that maps each station to the lidar system(s) used over a range of years.

### Input Files
- lev2db: A data frame containing metadata for Level 2 files (must be loaded beforehand).
- NetCDF files located in ./New/<station>/.

### Output
- syst_df: A data frame where:
  - Rows represent stations.
  - Columns represent years.
  - Each cell contains a semicolon-separated list of unique lidar systems used in that year.

### Logical Flow
The logical flow of the script is as follows:

- Initialization:
  - Extracts the list of unique stations from lev2db.
  - Initializes an empty character matrix syst with dimensions:
    - Rows: number of stations
    - Columns: number of years in the release range
    
- Metadata Extraction Loop:
  - For each station and year:
      - Filters lev2db for matching entries.
      - For each file:
        - Opens the corresponding NetCDF file.
        - Extracts the "system" global attribute.
        - Closes the file.
      - Stores the unique system names (concatenated with ;) in the matrix.

- Finalization: 
  - Converts the matrix to a data frame syst_df.
  - Assigns station names as row names and years as column names.

### Notes
- The script assumes that the "system" attribute exists in all NetCDF files.
- The year is inferred by adding 1999 to the loop index j, assuming the first year is 2000.
- The script is designed to be run after lev2db and release are defined.


## lev3pro_355.R  

### Purpose
This script processes Level 2 lidar data at 355 nm (and 351 nm for station "laq") to generate Level 3 profile datasets. It extracts, filters, and aggregates extinction, backscatter, and volume depolarization profiles, producing monthly, seasonal, and yearly statistics, including weighted medians and standard deviations.

### Input Files
- lev2db: Metadata table of Level 2 files.
- NetCDF files in ./New/<station>/.
- Utility functions: sson1, sson2, cnt, unq, len_0, w_median, w_sd.

### Output
- db_prof_month_355: Monthly profile statistics.
- db_prof_season_355: Seasonal profile statistics.
- db_prof_year_355: Yearly profile statistics.
- db_prof_nm_355: Normalized monthly statistics.
- db_prof_ns_355: Normalized seasonal statistics.

### Logical Flow
The logical flow of the script is as follows:

- Data Filtering:
  - Selects files with wavelength "0355" or "0351" (only for station "laq").
  
- File Processing Loop:
  - For each file:
    - Opens the NetCDF file and extracts:
      - altitude, extinction, backscatter, volumedepolarization, and their errors.
    - Converts altitude to meters if needed and filters valid range (100–12100 m).
    - Aligns altitudes to standard bins (every 200 m).
    - Stores extracted values in a temporary data frame.
    
- Data Cleaning:
  - Removes rows with all NA in key variables.
  - Adds Season and Season_Year columns.
  - Replaces infinite values with NA.
  
- Monthly Aggregation:
  - Computes mean, count, and unique profile counts per month and altitude.
  - Combines into db_prof_month_355.

- Seasonal Aggregation:
  - Computes:
    - Mean, median, standard deviation.
    - Count and unique profile counts.
  - Combines into db_prof_season_355.

- Yearly Aggregation:
  - Calculates weights based on number of measurements and months.
  - Computes:
    - Mean, weighted median, weighted standard deviation.
    - Count and unique profile counts.
  - Combines into db_prof_year_355.
  
- Normalized Monthly Aggregation:
  - Aggregates across months (ignoring year).
  - Computes weighted statistics and combines into db_prof_nm_355.
  
- Normalized Seasonal Aggregation:
  - Aggregates across seasons (ignoring year).
  - Computes weighted statistics and combines into db_prof_ns_355.

### Notes
- Extensive use of try() ensures robustness against missing variables.
- Weighting is based on inverse frequency of measurements and months/seasons.


## lev3pro_532.R

### Purpose
This script processes Level 2 lidar data at 532 nm to generate Level 3 profile datasets. It extracts extinction, backscatter, and volume depolarization profiles from NetCDF files, performs quality checks, and computes monthly, seasonal, and yearly statistics, including weighted medians and standard deviations.

### Input Files
- lev2db: Metadata table of Level 2 files.
- NetCDF files in ./New/<station>/.
- Utility functions: sson1, sson2, cnt, unq, len_0, w_median, w_sd.

### Output
- db_prof_month_532: Monthly profile statistics.
- db_prof_season_532: Seasonal profile statistics.
- db_prof_year_532: Yearly profile statistics.
- db_prof_nm_532: Normalized monthly statistics.
- db_prof_ns_532: Normalized seasonal statistics.

### Logical Flow
The logical flow of the script is as follows:

- Data Filtering:
  - Selects files with wavelength "0532".

- File Processing Loop:
  - For each file:
    - Opens the NetCDF file and extracts:
      - altitude, extinction, backscatter, volumedepolarization, and their errors.
    - Converts altitude to meters if needed and filters valid range (100–12100 m).
    - Aligns altitudes to standard bins (every 200 m).
    - Stores extracted values in a temporary data frame.

- Data Cleaning:
  - Removes rows with all NA in key variables.
  - Adds Season and Season_Year columns.
  - Replaces infinite values with NA.

- Monthly Aggregation:
  - Computes mean, count, and unique profile counts per month and altitude.
  - Combines into db_prof_month_532.

- Seasonal Aggregation:
  - Computes:
    - Mean, median, standard deviation
    - Count and unique profile counts
  - Combines into db_prof_season_532.

- Yearly Aggregation:
  - Calculates weights based on number of measurements and months.
  - Computes:
    - Mean, weighted median, weighted standard deviation
    - Count and unique profile counts
  - Combines into db_prof_year_532.

- Normalized Monthly Aggregation:
  - Aggregates across months (ignoring year).
  - Computes weighted statistics and combines into db_prof_nm_532.

- Normalized Seasonal Aggregation:
  - Aggregates across seasons (ignoring year).
  - Computes weighted statistics and combines into db_prof_ns_532.

### Notes
- Extensive use of try() ensures robustness against missing variables.
- Weighting is based on inverse frequency of measurements and months/seasons.
- The structure and logic are nearly identical to the 355 nm processing script, ensuring consistency across wavelengths.


## lev3pro_1064.R

### Purpose
This script processes Level 2 lidar data at 1064 nm to generate Level 3 profile datasets. It extracts backscatter and volume depolarization profiles from NetCDF files, performs quality checks, and computes monthly, seasonal, and yearly statistics, including weighted medians and standard deviations.

### Input Files
- lev2db: Metadata table of Level 2 files.
- NetCDF files in ./New/<station>/.
- Utility functions: sson1, sson2, cnt, unq, len_0, w_median, w_sd.

### Output
- db_prof_month_1064: Monthly profile statistics.
- db_prof_season_1064: Seasonal profile statistics.
- db_prof_year_1064: Yearly profile statistics.
- db_prof_nm_1064: Normalized monthly statistics.
- db_prof_ns_1064: Normalized seasonal statistics.

### Logical Flow
The logical flow of the script is as follows:

- Data Filtering:
  - Selects files with wavelength "1064".

- File Processing Loop:
  - For each file:
    - Opens the NetCDF file and extracts:
      - altitude, backscatter, volumedepolarization, and their errors.
    - Converts altitude to meters if needed and filters valid range (100–12100 m).
    - Aligns altitudes to standard bins (every 200 m).
    - Stores extracted values in a temporary data frame.

- Data Cleaning:
  - Removes rows with all NA in key variables.
  - Adds Season and Season_Year columns.
  - Replaces infinite values with NA.

- Monthly Aggregation:
  - Computes mean, count, and unique profile counts per month and altitude.
  - Combines into db_prof_month_1064.

- Seasonal Aggregation:
  - Computes:
    - Mean, median, standard deviation
    - Count and unique profile counts
  - Combines into db_prof_season_1064.

- Yearly Aggregation:
  - Calculates weights based on number of measurements and months.
  - Computes:
    - Mean, weighted median, weighted standard deviation
    - Count and unique profile counts
  - Combines into db_prof_year_1064.

- Normalized Monthly Aggregation:
  - Aggregates across months (ignoring year).
  - Computes weighted statistics and combines into db_prof_nm_1064.

- Normalized Seasonal Aggregation:
  - Aggregates across seasons (ignoring year).
  - Computes weighted statistics and combines into db_prof_ns_1064.

### Notes
- Unlike the 355 and 532 nm scripts, this one does not include extinction data.
- Weighting is based on inverse frequency of measurements and months/seasons.
- The structure is consistent with other wavelength scripts, ensuring comparability.


## lev3pro_nm_files.R

### Purpose
This script generates NetCDF files containing monthly averaged lidar profiles (Level 3) for each station. It processes extinction, backscatter, and volume depolarization data at three wavelengths (355, 532, 1064 nm), formats them into structured arrays, and writes them into standardized NetCDF files with appropriate metadata and attributes.

### Input
- Processed monthly profile datasets:
  - db_prof_nm_355, db_prof_nm_532, db_prof_nm_1064
- Station metadata: loc, latit, longit, institution, acronym, PI, PI_contact, sist
- Time range: release
- Utility functions: is_leap_year

### Output
- One NetCDF file per station:
  - Format: ACTRIS_AerRemSen_<station>_Lev03_NorMon<release_suffix>Pro_v03_qc040.nc
  - Saved in: ./Level3/Profiles/<station>/

### Logical Flow
The logical flow of the script is as follows:

- Data Preparation:
  - Rounds numeric values in the profile datasets to 6 significant digits.
  - Defines altitude levels (200–12000 m, every 200 m).
  - Computes monthly time bounds and central timestamps for the entire release period.

- Data Structuring:
  - For each station:
    - Initializes 4D arrays for:
      - extinction_data, backscatter_data, volume_depolarization_data
      - Dimensions: altitude × month × wavelength × statistics
    - Merges monthly data with altitude reference (appo) and fills arrays with:
      - Mean, error, median, standard deviation, and number of profiles

- NetCDF File Definition:
  - Defines NetCDF dimensions:
    - altitude, time, wavelength, stats, nv, n_char
  - Defines NetCDF variables:
    - extinction, backscatter, volume_depolarization, time_bounds, source, latitude, longitude, station_altitude

- Metadata and Attributes:
  - Adds standard CF-compliant attributes to dimensions and variables.
  - Adds global attributes:
    - Processor info, station metadata, PI and institution details, data originator/provider, conventions, references, etc.

- File Writing:
  - Writes all data and metadata into the NetCDF file.
  - Moves the file to the appropriate station-specific directory.

### Notes
- The script uses `signif()` to reduce numerical precision for storage efficiency.
- The stats dimension encodes:
  - 0: mean, 1: error, 2: median, 3: standard deviation, 4: number of profiles
- The script assumes that the profile datasets are already pre-processed and complete.


## lev3pro_ns_files.R

### Purpose
This script generates seasonally averaged Level 3 lidar profile NetCDF files for each station. It processes extinction, backscatter, and volume depolarization data at three wavelengths (355, 532, 1064 nm), organizes them into structured arrays, and writes them into standardized NetCDF files with appropriate metadata and attributes.

### Input
- Processed seasonal profile datasets:
  - db_prof_ns_355, db_prof_ns_532, db_prof_ns_1064
- Station metadata: loc, latit, longit, station_alt, institution, acronym, PI, PI_contact, sist
- Time range: release
- Utility functions: is_leap_year

### Output
- One NetCDF file per station:
  - Format: ACTRIS_AerRemSen_<station>_Lev03_NorSea<release_suffix>Pro_v03_qc040.nc
  - Saved in: ./Level3/Profiles/<station>/

### Logical Flow
The logical flow of the script is as follows:

- Data Preparation:
  - Rounds numeric values in the profile datasets to 6 significant digits.
  - Defines altitude levels (200–12000 m, every 200 m).
  - Computes seasonal time bounds and central timestamps for the entire release period.

- Data Structuring:
  - For each station:
    - Initializes 4D arrays for:
      - extinction_data, backscatter_data, volume_depolarization_data
      - Dimensions: altitude × season × wavelength × statistics
    - Merges seasonal data with altitude reference (appo) and fills arrays with:
      - Mean, error, median, standard deviation, and number of profiles
    - Handles missing data by padding with NA and replacing with NetCDF fill value.

- NetCDF File Definition:
  - Defines NetCDF dimensions:
    - altitude, time, wavelength, stats, nv, n_char
  - Defines NetCDF variables:
    - extinction, backscatter, volume_depolarization, time_bounds, source, latitude, longitude, station_altitude

- Metadata and Attributes:
  - Adds standard CF-compliant attributes to dimensions and variables.
  - Adds global attributes:
    - Processor info, station metadata, PI and institution details, data originator/provider, conventions, references, etc.

- File Writing:
  - Writes all data and metadata into the NetCDF file.
  - Moves the file to the appropriate station-specific directory.

### Notes
- The stats dimension encodes:
  - 0: mean, 1: error, 2: median, 3: standard deviation, 4: number of profiles
- The script ensures that each seasonal array has consistent dimensions (60 altitude bins).
- The structure is nearly identical to lev3pro_nm_files, but adapted for seasonal data.


## lev3pro_s_files.R

### Purpose
This script generates year-specific seasonal Level 3 NetCDF files for each station. It processes extinction, backscatter, and volume depolarization data at three wavelengths (355, 532, 1064 nm), organizes them into structured arrays by season and year, and writes them into standardized NetCDF files with appropriate metadata and attributes.

### Input
- Seasonal profile datasets:
  - db_prof_season_355, db_prof_season_532, db_prof_season_1064
- Station metadata: loc, latit, longit, station_alt, institution, acronym, PI, PI_contact, syst
- Time range: release
- Utility functions: is_leap_year, days_in_february

### Output
- One NetCDF file per station per year:
  - Format: ACTRIS_AerRemSen_<station>_Lev03_Season_<year>_Pro_v03_qc040.nc
  - Saved in: ./Level3/Profiles/<station>/

### Logical Flow
The logical flow of the script is as follows:

- Data Preparation:
  - Rounds numeric values in the seasonal profile datasets to 6 significant digits.
  - Defines altitude levels (200–12000 m, every 200 m).
  - Defines the four seasons: MarAprMay, JunJulAug, SepOctNov, DecJanFeb.

- Data Structuring:
  - For each station and year:
    - Filters seasonal data for the current year.
    - Initializes 4D arrays for:
      - extinction_data, backscatter_data, volume_depolarization_data
      - Dimensions: altitude × season × wavelength × statistics
    - Merges seasonal data with altitude reference (appo) and fills arrays with:
      - Mean, error, median, standard deviation, and number of profiles
    - Handles missing data by padding with NA and replacing with NetCDF fill value.

- Time Bounds:
  - Computes seasonal time bounds for the current year, including leap year handling for February.

- NetCDF File Definition:
  - Defines NetCDF dimensions:
    - altitude, time, wavelength, stats, nv, n_char
  - Defines NetCDF variables:
    - extinction, backscatter, volume_depolarization, time_bounds, source, latitude, longitude, station_altitude

- Metadata and Attributes:
  - Adds standard CF-compliant attributes to dimensions and variables.
  - Adds global attributes:
    - Processor info, station metadata, PI and institution details, data originator/provider, conventions, references, etc.

- File Writing:
  - Writes all data and metadata into the NetCDF file.
  - Moves the file to the appropriate station-specific directory.

### Notes
- The stats dimension encodes:
  - 0: mean, 1: error, 2: median, 3: standard deviation, 4: number of profiles
- The script ensures that each seasonal array has consistent dimensions (60 altitude bins).
- This script differs from lev3pro_ns_files by generating one file per year, rather than a single file for all years.


## lev3pro_y_files.R

### Purpose
This script generates annual Level 3 NetCDF files for each station and year. It processes extinction, backscatter, and volume depolarization data at three wavelengths (355, 532, 1064 nm), organizes them into structured arrays, and writes them into standardized NetCDF files with appropriate metadata and attributes.

### Input
- Annual profile datasets:
    - db_prof_year_355, db_prof_year_532, db_prof_year_1064
- Station metadata: loc, latit, longit, station_alt, institution, acronym, PI, PI_contact, syst
- Time range: release
- Utility functions: signif, subset

### Output
- One NetCDF file per station per year:
  - Format: ACTRIS_AerRemSen_<station>_Lev03_Annual_<year>_Pro_v03_qc040.nc
  - Saved in: ./Level3/Profiles/<station>/

### Logical Flow
The logical flow of the script is as follows:

- Data Preparation: 
  - Rounds numeric values in the annual profile datasets to 6 significant digits.
  - Defines altitude levels (200–12000 m, every 200 m).

- Data Structuring:
  - For each station and year:
    - Filters annual data for the current year.
    - Initializes 3D arrays for:
      - extinction_data, backscatter_data, volume_depolarization_data
      - Dimensions: altitude × wavelength × statistics
    - Merges data with altitude reference (appo) and fills arrays with:
      - Mean, error, median, standard deviation, and number of profiles
    - Handles missing data by replacing with NetCDF fill value.

- Time Bounds:
  - Defines a single time point (mid-year) and bounds (start and end of the year).

- NetCDF File Definition:
  - Defines NetCDF dimensions:
    - altitude, time, wavelength, stats, nv, n_char
  - Defines NetCDF variables:
    - extinction, backscatter, volume_depolarization, time_bounds, source, latitude, longitude, station_altitude

- Metadata and Attributes:
  - Adds standard CF-compliant attributes to dimensions and variables.
  - Adds global attributes:
    - Processor info, station metadata, PI and institution details, data originator/provider, conventions, references, etc.

- File Writing:
  - Writes all data and metadata into the NetCDF file.
  - Moves the file to the appropriate station-specific directory.

### Notes
- The stats dimension encodes:
  - 0: mean, 1: error, 2: median, 3: standard deviation, 4: number of profiles
- This script is similar in structure to lev3pro_s_files, but aggregates data over the full year instead of by season.


## lev3int_b_e_pbl.R

### Purpose
This script processes Level 2 lidar files to compute integrated aerosol quantities and derive Level 3 statistics. It calculates integrated backscatter, extinction, lidar ratio, particle depolarization, PBL height, and Angstrom exponent for different atmospheric layers (total, PBL, and free troposphere).

### Input
- lev2db: Metadata table of Level 2 files.
- lev2db_layers: Layer information (base and top altitudes).
- NetCDF files in ./New/<station>/.
- Utility functions: ang(), ang_err(), cnt_int(), etc.

### Output
- db_int_b: Integrated backscatter statistics.
- db_int_e: Integrated extinction and lidar ratio statistics.
- db_pd: Integrated particle depolarization statistics.
- db_pbl: PBL height estimates.
- db_angstrom: Angstrom exponent derived from 355/532 nm AOD.
  
### Logical Flow
The logical flow of the script is as follows:

- Initialization:
  - Preallocates data frames for:
    - Backscatter (db_int_b)
    - Extinction (db_int_e)
    - Particle depolarization (db_pd)
    - PBL height (db_pbl)

- File Loop:
  - For each file in lev2db:
    - Opens the corresponding NetCDF file.
    - Extracts altitude, backscatter, extinction, lidar ratio, and depolarization variables.
    - Converts altitude to meters if needed.
    - Retrieves PBL height from lev2db_layers or NetCDF variable.

- Backscatter Integration:
  - Applies trapezoidal integration to compute:
    - Total, PBL, and FT integrated backscatter
    - Center of mass for each region
    - Associated errors
  - Stores results in db_int_b.

- Extinction and Lidar Ratio Integration:
  - Applies trapezoidal integration to extinction profiles to compute:
    - Total, PBL, and FT AOD
    - Associated errors
  - Computes mean lidar ratio in each region.
  - Stores results in db_int_e.

- Particle Depolarization:
  - Computes mean depolarization in total, PBL, and FT layers.
  - Stores results in db_pd.

- PBL Height:
  - Extracts or estimates PBL height and stores in db_pbl.

- Angstrom Exponent:
  - Filters 355 nm extinction data with valid AOD.
  - Matches with corresponding 532 nm data by station and timestamp.
  - Computes Angstrom exponent and error for:
   - Total, PBL, and FT AOD
  - Stores results in db_angstrom.

- Finalization:
  - Cleans up NA and infinite values.
  - Aggregates db_pd and db_pbl by timestamp.
  - Sorts and resets row indices.

### Notes
- The script includes both active and alternative (commented) code for integration.
- Uses trapezoidal rule for numerical integration.
- Handles missing or malformed NetCDF variables gracefully using try().


## lev3int_355.R

### Purpose
This script processes integrated aerosol data at 355 nm (and 351 nm for station "laq") from Level 2 files to compute Level 3 statistics. It calculates monthly, seasonal, yearly, and climatological (normal) means, medians, standard deviations, and counts for both extinction and backscatter data, as well as for particle depolarization and PBL (Planetary Boundary Layer) height.

### Input
- db_int_e, db_int_b: Integrated extinction and backscatter datasets.
- db_pd: Particle depolarization dataset.
- db_pbl: PBL height dataset.
- Utility functions: cnt_int, ws, w_median_int, w_sd_int, seas, seas1.

### Output
- Aggregated data frames for:
  - Monthly, seasonal, yearly, and normal (climatological) statistics.
  - Separate outputs for extinction (db_e_* ) and backscatter (db_b_*).
  - Particle depolarization (db_pd_* ) and PBL height (db_pbl_*).

### Logical Flow
The logical flow of the script is as follows:

- Monthly Aggregation:
  - Filters and combines 355 nm and 351 nm data.
  - Computes:
    - Mean values (*_m_mean)
    - Count of valid values (*_m_cnt)

- Yearly Aggregation:
  - Aggregates monthly data to yearly statistics.
  - Computes:
    - Mean, weighted median, weighted standard deviation
    - Count of valid values
  - Combines results into final yearly datasets: db_e_y_355, db_b_y_355

- Seasonal Aggregation:
  - Assigns Season_Year using `seas()`.
  - Computes:
    - Mean, median, standard deviation, count
  - Combines results into: db_e_s_355, db_b_s_355
  
- Normal Monthly Aggregation:
  - Aggregates across all years by month.
  - Computes:
    - Mean, weighted median, weighted standard deviation, count
  - Combines into: db_e_nm_355, db_b_nm_355

- Normal Seasonal Aggregation:
  - Aggregates across all years by season.
  - Computes:
    - Mean, weighted median, weighted standard deviation, count
  - Combines into: db_e_ns_355, db_b_ns_355

- PBL Statistics:
  - Computes monthly, yearly, seasonal, and normal statistics for PBL height.
  - Uses similar structure as above (mean, median, standard deviation, count).
  - Final outputs: db_pbl_m_mean, db_pbl_y_tot, db_pbl_s_tot, db_pbl_nm_tot, db_pbl_ns_tot

- Particle Depolarization Statistics:
  - Filters 355 nm data from db_pd.
  - Computes:
    - Monthly, yearly, seasonal, and normal statistics
    - Mean, median, standard deviation, count
  - Final outputs: db_pd_m_mean, db_pd_y_355, db_pd_s_355, db_pd_nm_355, db_pd_ns_355

### Notes
- The script uses `mapply()` with custom weighting functions for robust statistical aggregation.
- All outputs are structured and named consistently for downstream use in NetCDF generation.
- The script handles both standard and special cases (e.g., station "laq" with 351 nm).


## lev3int_532.R

### Purpose
This script processes integrated aerosol data at 532 nm from Level 2 files to compute Level 3 statistics. It calculates monthly, seasonal, yearly, and climatological (normal) means, medians, standard deviations, and counts for both extinction and backscatter data, as well as for particle depolarization (PartDep).

### Input
- db_int_e, db_int_b: Integrated extinction and backscatter datasets.
- db_pd: Particle depolarization dataset.
- Utility functions: cnt_int, ws, w_median_int, w_sd_int, seas, seas1.

### Output
- Aggregated data frames for:
  - Monthly, seasonal, yearly, and normal (climatological) statistics.
- Separate outputs for extinction (db_e_* ) and backscatter (db_b_*).
- Particle depolarization (db_pd_*).

### Logical Flow
The logical flow of the script is as follows:

- Monthly Aggregation: 
  - Filters 532 nm data from db_int_e and db_int_b.
  - Computes:
    - Mean values (*_m_mean)
    - Count of valid values (*_m_cnt)

- Yearly Aggregation:
  - Aggregates monthly data to yearly statistics.
  - Computes:
    - Mean, weighted median, weighted standard deviation
    - Count of valid values
  - Combines results into final yearly datasets: db_e_y_532, db_b_y_532

- Seasonal Aggregation:
  - Assigns Season_Year using `seas()`.
  - Computes:
    - Mean, median, standard deviation, count
  - Combines results into: db_e_s_532, db_b_s_532

- Normal Monthly Aggregation:
  - Aggregates across all years by month.
  - Computes:
    - Mean, weighted median, weighted standard deviation, count
  - Combines into: db_e_nm_532, db_b_nm_532

- Normal Seasonal Aggregation:
  - Aggregates across all years by season.
  - Computes:
    - Mean, weighted median, weighted standard deviation, count
  - Combines into: db_e_ns_532, db_b_ns_532

- Particle Depolarization Statistics:
  - Filters 532 nm data from db_pd.
  - Computes:
    - Monthly, yearly, seasonal, and normal statistics
    - Mean, median, standard deviation, count
  - Final outputs:
    - db_pd_m_mean, db_pd_y_532, db_pd_s_532, db_pd_nm_532, db_pd_ns_532

### Notes
- The script uses `mapply()` with custom weighting functions for robust statistical aggregation.
- All outputs are structured and named consistently for downstream use in NetCDF generation.
- The structure is nearly identical to the 355 nm script (lev3int_355.R), ensuring consistency across wavelengths.


## lev3int_1064.R

### Purpose
This script processes integrated aerosol data at 1064 nm from Level 2 files to compute Level 3 statistics. It calculates monthly, seasonal, yearly, and climatological (normal) means, medians, standard deviations, and counts for backscatter and volume depolarization data, as well as for Angstrom exponent and particle depolarization (PartDep).

### Input
- db_int_b: Integrated backscatter dataset.
- db_pd: Particle depolarization dataset.
- db_angstrom: Angstrom exponent dataset.
- Utility functions: cnt_int, ws, w_median_int, w_sd_int, seas, seas1.

### Output
- Aggregated data frames for:
  - Monthly, seasonal, yearly, and normal (climatological) statistics.
  - Backscatter: db_b_*_1064
  - PartDep: db_pd_*_1064
  - Angstrom exponent: db_ang_*_tot

### Logical Flow
The logical flow of the script is as follows:

- Monthly Aggregation: 
  - Filters 1064 nm data from db_int_b.
  - Computes:
    - Mean values (db_b_m_mean)
    - Count of valid values (db_b_m_cnt)

- Yearly Aggregation:
  - Aggregates monthly data to yearly statistics.
  - Computes:
    - Mean, weighted median, weighted standard deviation
    - Count of valid values
  - Combines results into: db_b_y_1064

- Seasonal Aggregation:
  - Assigns Season_Year using seas().
  - Computes:
    - Mean, median, standard deviation, count
  - Combines into: db_b_s_1064

- Normal Monthly Aggregation: 
  - Aggregates across all years by month.
  - Computes:
    - Mean, weighted median, weighted standard deviation, count
  - Combines into: db_b_nm_1064

- Normal Seasonal Aggregation:
  - Aggregates across all years by season.
  - Computes:
    - Mean, weighted median, weighted standard deviation, count
  - Combines into: db_b_ns_1064

- Angstrom Exponent Statistics:
  - Processes db_angstrom data.
  - Computes:
    - Monthly, yearly, seasonal, and normal statistics
    - Mean, median, standard deviation, count
  - Combines into: db_ang_*_tot

- Particle Depolarization Statistics:
  - Filters 1064 nm data from db_pd.
  - Computes:
    - Monthly, yearly, seasonal, and normal statistics
    - Mean, median, standard deviation, count
  - Combines into: db_pd_*_1064

### Notes
- The script uses `mapply()` with custom weighting functions for robust statistical aggregation.
- All outputs are structured and named consistently for downstream use in NetCDF generation.
- The structure is consistent with the 355 and 532 nm scripts, ensuring comparability across wavelengths.


## lev3int_nm_files.R

### Purpose
This script generates monthly climatological (normal) NetCDF files containing integrated aerosol measurements for each station. It processes data at 355, 532, and 1064 nm wavelengths, including aerosol optical depth (AOD), integrated backscatter, lidar ratio, particle depolarization, Angstrom exponent, and PBL height.

### Input
- Monthly climatological datasets:
  - db_e_nm_* , db_b_nm_* , db_pd_nm_* for extinction, backscatter, and depolarization
  - db_ang_nm_tot for Angstrom exponent
  - db_pbl_nm_tot for PBL height
- Station metadata: loc, latit, longit, station_alt, institution, acronym, PI, PI_contact, sist
- Time range: release
- Utility functions: filter_and_merge(), is_leap_year()

### Output
- One NetCDF file per station:
  - Format: ACTRIS_AerRemSen_<station>_Lev03_NorMon<release_suffix>Int_v03_qc040.nc
  - Saved in: ./Level3/Integrated/<station>/

### Logical Flow
The logical flow of the script is as follows:

- Data Preparation:
  - Rounds numeric values to 6 significant digits.
  - Defines time bounds and central timestamps for each month.
  - Prepares a reference data frame for merging by month.

- Data Structuring:
  - For each station:
    - Filters and merges monthly data for each wavelength and variable.
    - Initializes and fills multidimensional arrays for:
      - Aerosol Optical Depth: [time, bounds, wavelength, stats]
      - Lidar Ratio: [time, bounds, wavelength, stats]
      - Integrated Backscatter: [time, bounds, wavelength, stats]
      - Center of Mass: [time, bounds, wavelength, stats]
      - Particle Depolarization: [time, bounds, wavelength, stats]
      - Angstrom Coefficient: [time, bounds, stats]
      - PBL Height: [time, stats]
      - H63 of AOD and Backscatter: [time, wavelength, stats]

- NetCDF File Definition:
  - Defines NetCDF dimensions:
    - time, nv (bounds), wavelength, stats, n_char
  - Defines NetCDF variables with appropriate units and long names.

- Metadata and Attributes:
  - Adds CF-compliant attributes to dimensions and variables.
  - Adds global attributes:
    - Processor info, station metadata, PI and institution details, data originator/provider, conventions, references, etc.

- File Writing:
  - Writes all data and metadata into the NetCDF file.
  - Moves the file to the appropriate station-specific directory.

### Notes
- The stats dimension encodes:
  - 0: mean, 1: error, 2: median, 3: standard deviation, 4: number of values
- The nv dimension encodes:
  - 0: total, 1: aerosol boundary layer
- Missing values are filled with the NetCDF default fill value (9.9692099683868690e+36).
- The script ensures consistency across wavelengths and variables.


## lev3int_ns_files.R

### Purpose
This script generates seasonal climatological (normal) NetCDF files containing integrated aerosol measurements for each station. It processes data at 355, 532, and 1064 nm wavelengths, including aerosol optical depth (AOD), integrated backscatter, lidar ratio, particle depolarization, Angstrom exponent, and PBL height.

### Input
- Seasonal climatological datasets:
  - db_e_ns_* , db_b_ns_* , db_pd_ns_* for extinction, backscatter, and depolarization
  - db_ang_ns_tot for Angstrom exponent
  - db_pbl_ns_tot for PBL height
- Station metadata: loc, latit, longit, station_alt, institution, acronym, PI, PI_contact, sist
- Time range: release
- Utility functions: filter_and_merge_seas(), is_leap_year()

### Output
- One NetCDF file per station:
  - Format: ACTRIS_AerRemSen_<station>_Lev03_NorSea<release_suffix>Int_v03_qc040.nc
  - Saved in: ./Level3/Integrated/<station>/
  
### Logical Flow
The logical flow of the script is as follows:

- Data Preparation:
  - Rounds numeric values to 6 significant digits.
  - Defines time bounds and central timestamps for each season.
  - Prepares a reference data frame for merging by season.

- Data Structuring:
  - For each station:
    - Filters and merges seasonal data for each wavelength and variable.
    - Initializes and fills multidimensional arrays for:
      - Aerosol Optical Depth: [season, bounds, wavelength, stats]
      - Lidar Ratio: [season, bounds, wavelength, stats]
      - Integrated Backscatter: [season, bounds, wavelength, stats]
      - Center of Mass: [season, bounds, wavelength, stats]
      - Particle Depolarization: [season, bounds, wavelength, stats]
      - Angstrom Coefficient: [season, bounds, stats]
      - PBL Height: [season, stats]
      - H63 of AOD and Backscatter: [season, wavelength, stats]

- NetCDF File Definition:
  - Defines NetCDF dimensions:
    - time, nv (bounds), wavelength, stats, n_char
  - Defines NetCDF variables with appropriate units and long names.

- Metadata and Attributes:
  - Adds CF-compliant attributes to dimensions and variables.
  - Adds global attributes:
    - Processor info, station metadata, PI and institution details, data originator/provider, conventions, references, etc.

- File Writing:
  - Writes all data and metadata into the NetCDF file.
  - Moves the file to the appropriate station-specific directory.

### Notes
- The stats dimension encodes:
  - 0: mean, 1: error, 2: median, 3: standard deviation, 4: number of values
- The nv dimension encodes:
  - 0: total, 1: aerosol boundary layer
- Missing values are filled with the NetCDF default fill value (9.9692099683868690e+36).
- The script ensures consistency across wavelengths and variables.


## lev3int_s_files.R  

### Purpose
This script generates year-specific seasonal NetCDF files containing integrated aerosol measurements for each station. It processes data at 355, 532, and 1064 nm wavelengths, including aerosol optical depth (AOD), integrated backscatter, lidar ratio, particle depolarization, Angstrom exponent, and PBL height.

### Input
- Seasonal datasets by year:
  - db_e_s_* , db_b_s_* , db_pd_s_* for extinction, backscatter, and depolarization
  - db_ang_s_tot for Angstrom exponent
  - db_pbl_s_tot for PBL height
- Station metadata: loc, latit, longit, station_alt, institution, acronym, PI, PI_contact, syst
- Time range: release
- Utility functions: filter_and_merge_seas_y(), days_in_february()

### Output
- One NetCDF file per station per year:
  - Format: ACTRIS_AerRemSen_<station>_Lev03_Season_<year>_Int_v03_qc040.nc
  - Saved in: ./Level3/Integrated/<station>/

### Logical Flow
The logical flow of the script is as follows:

- Data Preparation:
  - Rounds numeric values to 6 significant digits.
  - Defines seasonal time bounds and central timestamps for each year.
  - Prepares a reference data frame for merging by Season_Year.

- Data Structuring:
  - For each station and year:
    - Filters and merges seasonal data for each wavelength and variable.
    - Initializes and fills multidimensional arrays for:
      - Aerosol Optical Depth: [season, bounds, wavelength, stats]
      - Lidar Ratio: [season, bounds, wavelength, stats]
      - Integrated Backscatter: [season, bounds, wavelength, stats]
      - Center of Mass: [season, bounds, wavelength, stats]
      - Particle Depolarization: [season, bounds, wavelength, stats]
      - Angstrom Coefficient: [season, bounds, stats]
      - PBL Height: [season, stats]
      - H63 of AOD and Backscatter: [season, wavelength, stats]

- NetCDF File Definition:
  - Defines NetCDF dimensions:
    - time, nv (bounds), wavelength, stats, n_char
  - Defines NetCDF variables with appropriate units and long names.

- Metadata and Attributes:
  - Adds CF-compliant attributes to dimensions and variables.
  - Adds global attributes:
    - Processor info, station metadata, PI and institution details, data originator/provider, conventions, references, etc.

- File Writing:
  - Writes all data and metadata into the NetCDF file.
  - Moves the file to the appropriate station-specific directory.

### Notes
- The stats dimension encodes:
  - 0: mean, 1: error, 2: median, 3: standard deviation, 4: number of values
- The nv dimension encodes:
  - 0: total, 1: aerosol boundary layer
- Missing values are filled with the NetCDF default fill value (9.9692099683868690e+36).
- This script differs from lev3int_ns_files by generating one file per year, rather than a single file for all years.


## lev3int_y_files.R

### Purpose
This script generates annual NetCDF files containing integrated aerosol measurements for each station and year. It processes data at 355, 532, and 1064 nm wavelengths, including aerosol optical depth (AOD), integrated backscatter, lidar ratio, particle depolarization, Angstrom exponent, and PBL height.

### Input
- Annual datasets:
  - db_e_y_* , db_b_y_* , db_pd_y_* for extinction, backscatter, and depolarization
  - db_ang_y_tot for Angstrom exponent
  - db_pbl_y_tot for PBL height
- Station metadata: loc, latit, longit, station_alt, institution, acronym, PI, PI_contact, syst
- Time range: release
- Utility functions: filter_and_merge_y()

### Output
- One NetCDF file per station per year:
  - Format: ACTRIS_AerRemSen_<station>_Lev03_Annual_<year>_Int_v03_qc040.nc
  - Saved in: ./Level3/Integrated/<station>/

### Logical Flow
The logical flow of the script is as follows:

- Data Preparation:
  - Rounds numeric values to 6 significant digits.
  - Defines a single time point (mid-year) and bounds (start and end of the year).

- Data Structuring:
  - For each station and year:
    - Filters and merges annual data for each wavelength and variable.
    - Initializes and fills multidimensional arrays for:
      - Aerosol Optical Depth: [bounds, wavelength, stats]
      - Lidar Ratio: [bounds, wavelength, stats]
      - Integrated Backscatter: [bounds, wavelength, stats]
      - Center of Mass: [bounds, wavelength, stats]
      - Particle Depolarization: [bounds, wavelength, stats]
      - Angstrom Coefficient: [bounds, stats]
      - PBL Height: [stats]
      - H63 of AOD and Backscatter: [wavelength, stats]

- NetCDF File Definition:
  - Defines NetCDF dimensions:
    - time, nv (bounds), wavelength, stats, n_char
  - Defines NetCDF variables with appropriate units and long names.

- Metadata and Attributes:
  - Adds CF-compliant attributes to dimensions and variables.
  - Adds global attributes:
    - Processor info, station metadata, PI and institution details, data originator/provider, conventions, references, etc.

- File Writing:
  - Writes all data and metadata into the NetCDF file.
  - Moves the file to the appropriate station-specific directory.

### Notes
- The stats dimension encodes:
  - 0: mean, 1: error, 2: median, 3: standard deviation, 4: number of values
- The nv dimension encodes:
  - 0: total, 1: aerosol boundary layer
- Missing values are filled with the NetCDF default fill value (9.9692099683868690e+36).
- This script is the annual counterpart to lev3int_s_files, aggregating data over the full year instead of by season.


## lev3int_layers.R

### Purpose
This script computes layer-resolved integrated aerosol properties from Level 2 NetCDF files. It processes each defined aerosol layer to calculate statistics such as integrated backscatter, extinction, AOD, lidar ratio, particle depolarization, and center of mass.

### Input
- lev2db: Metadata table of Level 2 files.
- lev2db_layers: Table of aerosol layer definitions (base and top altitudes).
- NetCDF files in ./New/<station>/.
- Utility functions: get_nc_var() for safe variable extraction.

### Output
- db_int_layers: A data frame containing integrated properties for each aerosol layer in each file.
  
### Logical Flow
The logical flow of the script is as follows:

- Initialization: 
  - Preallocates db_int_layers with 27 columns, including:
    - Layer metadata from lev2db_layers
  - Calculated variables: LidarRatio, ParticleDep, AOD, Extinction, CenterMass, IntBs, Backscatter and their errors

- File Loop:
  - For each file in lev2db:
    - Opens the corresponding NetCDF file.
    - Extracts altitude and aerosol variables:
      - backscatter, extinction, lidarratio, particledepolarization and their errors
    - Filters valid layers from lev2db_layers.

- Layer Loop:
  -For each aerosol layer:
    - Identifies the altitude range and extracts corresponding profile segments.
    - Filters valid data points based on quality criteria.
    - Computes:
      - Integrated Backscatter and Center of Mass using trapezoidal integration
      - AOD using extinction profiles
      - Weighted Means for backscatter, extinction, lidar ratio, and depolarization
      - Associated Errors using simplified propagation

- Data Aggregation:
  - Appends computed values to the corresponding layer row.
  - Merges all processed layers into db_int_layers.

- Post-processing:
  - Removes the initial placeholder row.
  - Ensures all NA values are explicitly set.
  - Corrects CenterMass values to ensure they lie within the layer bounds.

### Notes
- The script uses weighted means based on inverse relative error.
- Integration is performed using the trapezoidal rule.
- Layers with Cloud_Flag < 0.5 are considered for backscatter integration.
- The script is designed to be robust to missing or malformed NetCDF variables.


## lev3layers_nm_files.R

### Purpose
This script generates monthly climatological (normal) NetCDF files containing histograms of aerosol layer properties for each station. It processes layer-resolved data such as base/top altitudes, lidar ratio, extinction, AOD, particle depolarization, integrated backscatter, and center of mass.

### Input
- db_int_layers: Layer-resolved integrated data.
- layers_base_top: Base and top altitude definitions for aerosol layers.
- Histogram bin definitions: altitude_breaks, lr_breaks, pd_breaks, aod_breaks, extinction_breaks, ib_breaks, backscatter_breaks.
- Station metadata: loc, latit, longit, station_alt, institution, acronym, PI, PI_contact, sist
- Time range: release

### Output
- One NetCDF file per station:
  - Format: ACTRIS_AerRemSen_<station>_Lev03_NorMon<release_suffix>Lay_v03_qc040.nc
  - Saved in: ./Level3/Layers/<station>/

### Logical Flow
The logical flow of the script is as follows:

- Initialization: 
  - Prepares histogram count arrays for each variable and month:
    - bl_counts, tl_counts, lr_counts, pd_counts, aod_counts, extinction_counts, com_counts, ib_counts, backscatter_counts
  - Defines time bounds and central timestamps for each month.

- Data Aggregation:
  - For each station and month:
    - Filters db_int_layers and layers_base_top by station, month, and wavelength (355, 532, 1064).
    - Computes histograms for:
      - Base/Top Layer Altitudes
      - Lidar Ratio
      - Particle Depolarization
      - AOD
      - Extinction
      - Center of Mass
      - Integrated Backscatter
      - Backscatter
    - Stores histogram counts in corresponding arrays.

- NetCDF File Definition:
  - Defines NetCDF dimensions:
    - time, wavelength, breaks, nv, n_char
  - Defines histogram variables and their bin intervals.
  - Adds descriptive attributes for histogram interpretation.

- Metadata and Attributes:
  - Adds CF-compliant attributes to dimensions and variables.
  - Adds global attributes:
    - Processor info, station metadata, PI and institution details, data originator/provider, conventions, references, etc.

- File Writing: 
  - Writes all histogram data and metadata into the NetCDF file.
  - Moves the file to the appropriate station-specific directory.

### Notes
- Histogram bins are defined externally and referenced via attributes.
- The last histogram bin is right-open (no upper bound).
- Missing values are filled with the NetCDF default fill value (9.9692099683868690e+36).
- This script complements lev3int_nm_files by providing distributional rather than aggregated statistics.


## lev3layers_ns_files.R  

### Purpose
This script generates seasonal climatological (normal) NetCDF files containing histograms of aerosol layer properties for each station. It processes layer-resolved data such as base/top altitudes, lidar ratio, extinction, AOD, particle depolarization, integrated backscatter, and center of mass.

### Input
- db_int_layers: Layer-resolved integrated data.
- layers_base_top: Base and top altitude definitions for aerosol layers.
- Histogram bin definitions: altitude_breaks, lr_breaks, pd_breaks, aod_breaks, extinction_breaks, ib_breaks, backscatter_breaks.
- Station metadata: loc, latit, longit, station_alt, institution, acronym, PI, PI_contact, sist
- Time range: release

### Output
- One NetCDF file per station:
  - Format: ACTRIS_AerRemSen_<station>_Lev03_NorSea<release_suffix>Lay_v03_qc040.nc
  - Saved in: ./Level3/Layers/<station>/

### Logical Flow
The logical flow of the script is as follows:

- Initialization: 
  - Defines seasonal groupings of months.
  - Prepares histogram count arrays for each variable and season:
    - bl_counts, tl_counts, lr_counts, pd_counts, aod_counts, extinction_counts, com_counts, ib_counts, backscatter_counts
  - Defines time bounds and central timestamps for each season.

- Data Aggregation:
  - For each station and season:
    - Filters db_int_layers and layers_base_top by station, season, and wavelength (355, 532, 1064).
    - Computes histograms for:
      - Base/Top Layer Altitudes
      - Lidar Ratio
      - Particle Depolarization
      - AOD
      - Extinction
      - Center of Mass
      - Integrated Backscatter
      - Backscatter
    - Stores histogram counts in corresponding arrays.

- NetCDF File Definition:
  - Defines NetCDF dimensions:
    - time, wavelength, breaks, nv, n_char
  - Defines histogram variables and their bin intervals.
  - Adds descriptive attributes for histogram interpretation.

- Metadata and Attributes:
  - Adds CF-compliant attributes to dimensions and variables.
  - Adds global attributes:
    - Processor info, station metadata, PI and institution details, data originator/provider, conventions, references, etc.

- File Writing:
  - Writes all histogram data and metadata into the NetCDF file.
  - Moves the file to the appropriate station-specific directory.

### Notes
- Histogram bins are defined externally and referenced via attributes.
- The last histogram bin is right-open (no upper bound).
- Missing values are filled with the NetCDF default fill value (9.9692099683868690e+36).
- This script complements lev3int_ns_files by providing distributional rather than aggregated statistics.


## lev3layers_s_files.R

### Purpose
This script generates year-specific seasonal NetCDF files containing histograms of aerosol layer properties for each station. It processes layer-resolved data such as base/top altitudes, lidar ratio, extinction, AOD, particle depolarization, integrated backscatter, and center of mass.

### Input
- db_int_layers: Layer-resolved integrated data.
- layers_base_top: Base and top altitude definitions for aerosol layers.
- Histogram bin definitions: altitude_breaks, lr_breaks, pd_breaks, aod_breaks, extinction_breaks, ib_breaks, backscatter_breaks.
- Station metadata: loc, latit, longit, station_alt, institution, acronym, PI, PI_contact, syst
- Time range: release

### Output
- One NetCDF file per station per year:
  - Format: ACTRIS_AerRemSen_<station>_Lev03_Season_<year>_Lay_v03_qc040.nc
  - Saved in: ./Level3/Layers/<station>/

### Logical Flow
The logical flow of the script is as follows:

- Initialization:
  - Defines seasonal groupings of months.
  - Prepares histogram count arrays for each variable and season:
    - bl_counts, tl_counts, lr_counts, pd_counts, aod_counts, extinction_counts, com_counts, ib_counts, backscatter_counts

- Data Aggregation:
  - For each station and year:
    - Filters db_int_layers and layers_base_top by station, year, season, and wavelength (355, 532, 1064).
    - Computes histograms for:
      - Base/Top Layer Altitudes
      - Lidar Ratio
      - Particle Depolarization
      - AOD
      - Extinction
      - Center of Mass
      - Integrated Backscatter
      - Backscatter
    - Stores histogram counts in corresponding arrays.

- NetCDF File Definition:
  - Defines NetCDF dimensions:
    - time, wavelength, breaks, nv, n_char
  - Defines histogram variables and their bin intervals.
  - Adds descriptive attributes for histogram interpretation.

- Metadata and Attributes:
  - Adds CF-compliant attributes to dimensions and variables.
  - Adds global attributes:
    - Processor info, station metadata, PI and institution details, data originator/provider, conventions, references, etc.

- File Writing:
  - Writes all histogram data and metadata into the NetCDF file.
  - Moves the file to the appropriate station-specific directory.

### Notes
- Histogram bins are defined externally and referenced via attributes.
- The last histogram bin is right-open (no upper bound).
- Missing values are filled with the NetCDF default fill value (9.9692099683868690e+36).
- This script complements lev3int_s_files by providing distributional rather than aggregated statistics.


## lev3layers_y_files.R

### Purpose
This script generates annual NetCDF files containing histograms of aerosol layer properties for each station. It processes layer-resolved data such as base/top altitudes, lidar ratio, extinction, AOD, particle depolarization, integrated backscatter, and center of mass.

### Input
- db_int_layers: Layer-resolved integrated data.
- layers_base_top: Base and top altitude definitions for aerosol layers.
- Histogram bin definitions: altitude_breaks, lr_breaks, pd_breaks, aod_breaks, extinction_breaks, ib_breaks, backscatter_breaks.
- Station metadata: loc, latit, longit, station_alt, institution, acronym, PI, PI_contact, syst
- Time range: release

### Output
- One NetCDF file per station per year:
  - Format: ACTRIS_AerRemSen_<station>_Lev03_Annual_<year>_Lay_v03_qc040.nc
  - Saved in: ./Level3/Layers/<station>/

### Logical Flow
The logical flow of the script is as follows:

- Data Aggregation:
  - For each station and year:
    - Filters db_int_layers and layers_base_top by station, year, and wavelength (355, 532, 1064).
    - Computes histograms for:
      - Base/Top Layer Altitudes
      - Lidar Ratio
      - Particle Depolarization
      - AOD
      - Extinction
      - Center of Mass
      - Integrated Backscatter
      - Backscatter
    - Stores histogram counts in corresponding arrays.

- NetCDF File Definition:
  - Defines NetCDF dimensions:
    - time, wavelength, breaks, nv, n_char
  - Defines histogram variables and their bin intervals.
  - Adds descriptive attributes for histogram interpretation.

- Metadata and Attributes:
  - Adds CF-compliant attributes to dimensions and variables.
  - Adds global attributes:
    - Processor info, station metadata, PI and institution details, data originator/provider, conventions, references, etc.

- File Writing:
  - Writes all histogram data and metadata into the NetCDF file.
  - Moves the file to the appropriate station-specific directory.

### Notes
- Histogram bins are defined externally and referenced via attributes.
- The last histogram bin is right-open (no upper bound).
- Missing values are filled with the NetCDF default fill value (9.9692099683868690e+36).
- This script complements lev3int_y_files by providing distributional rather than aggregated statistics.



# SKS Anomaly for Blocking Detection

Computes the SKS Z500 anomaly and monthly 1σ thresholds, following the methodology of Sausen et al. (1995). The output anomaly field and threshold file are used as input for the subsequent blocking detection algorithm.

## Prerequisites

- Fortran compiler (Intel `ifort` recommended)
- NetCDF & HDF5 libraries
- CDO (Climate Data Operators)

## Input Data

A single NetCDF file of **daily 500 hPa geopotential height (Z500)** is required.

**Example format:**

| Attribute | Value |
|---|---|
| Variable name | `hgt` |
| Dimensions | `lon × lat × time` |
| Grid | 1° × 1°, global (360 × 181, 0–359°E, 90°S–90°N) |
| Calendar | noleap (365-day) |
| Time span | continuous daily record (e.g. 1979-01-01 to 2025-02-28) |

Example filename: `z500.day.19790101.20250228.noleap.nc`

> **Note:** If your data uses a different calendar (leap year, 360-day), or a different variable name, you must adjust the configuration accordingly (see below).

## Configuration

Before running, two files must be edited to match your data.

### `qq.h` — Grid and time parameters

```fortran
parameter (ix=360, iy=91, il=181, lim=16849, maxdaylim=16849)
parameter (nyr=47, nyear=1979, nmon=nyr*12)
parameter (llim=121, ulim=181)
parameter (latres=1.0, lonres=1.0, lowlat=-90.0)
parameter (maxday1=16849, yrtype1=2)
```

| Parameter | Description |
|---|---|
| `ix` | Number of longitude grid points |
| `iy` | Latitude index of the equator (used as loop start for NH) |
| `il` | Number of latitude grid points |
| `maxdaylim` | Maximum number of days across all datasets (array size) |
| `nyr` | Number of years in the dataset |
| `nyear` | Start year |
| `llim`, `ulim` | Latitude index range for threshold calculation (30°–90° by default) |
| `latres`, `lonres` | Grid resolution in degrees |
| `lowlat` | Latitude of the first grid point |
| `maxday1` | Total number of days in the input file |
| `yrtype1` | Calendar type: `1` = leap year, `2` = no leap year, `3` = 360-day |

### `sks.csh` — Paths, filenames, and CTL settings

Edit the following variables at the top of the script:

```csh
set infile1 = z500.day.19790101.20250228.noleap    # input filename (without .nc)
set InDir   = /path/to/input                        # directory containing input .nc
set DatDir  = /path/to/output                       # directory for output files
set SrcDir  = /path/to/source                       # directory containing source code
```

The CTL (GrADS descriptor) section near the bottom of the script controls the binary-to-NetCDF conversion. These values **must match** your `qq.h` settings:

```csh
set ix        = 360       # must match ix in qq.h
set il        = 181       # must match il in qq.h
set latres    = 1.0       # must match latres in qq.h
set lonres    = 1.0       # must match lonres in qq.h
set lowlat    = -90.0     # must match lowlat in qq.h
set syear     = 1979      # must match nyear in qq.h
set maxday    = 16849     # must match maxday1 in qq.h
```

The `OPTIONS 365_day_calendar` line in the CTL template should also be changed if your data uses a different calendar.

## Usage

```bash
csh sks.csh
```

This single command runs the full pipeline:

1. **Compile** — Builds `SKSanomaly.exe` from the Fortran sources via `make`
2. **Compute anomaly** — Runs `SKSanomaly.exe`, which reads the input NetCDF and writes binary output
3. **Post-process** — Converts the binary anomaly to NetCDF using CDO, and generates the monthly threshold (`cdo.monmean.ymonstd.nc`)

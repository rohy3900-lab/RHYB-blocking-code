# Blocking Detection Pipeline

Detects atmospheric blocking events from daily 500 hPa geopotential height (Z500) data.  
**Step 1 must be completed before running Step 2.**

```
[Z500 raw field]
      │
      ▼
┌─────────────┐
│  Step 1     │  sks/sks.csh
│  SKS        │  → Computes Z500 anomaly & monthly threshold
└──────┬──────┘
       │  SKS.z500.bin  +  cdo.monmean.ymonstd.nc
       ▼
┌─────────────┐
│  Step 2     │  main/main.csh
│  MAIN       │  → Detects blocking events
└──────┬──────┘
       │
       ▼
  bdate.txt  /  relabel.nc
```

---

## Prerequisites

- Fortran compiler (Intel `ifort` recommended)
- NetCDF & HDF5 libraries
- CDO (Climate Data Operators)

---

## Input Data

A single NetCDF file of **daily 500 hPa geopotential height (Z500)** is required.  
The values below are examples — dimension order and time span must be respected as-is.

| Attribute | Example | Note |
|---|---|---|
| Variable name | `hgt` | Must match `readnetcdf.F` line 16 |
| Dimensions | `time x lat × lon` | Order is fixed |
| Grid | 1° × 1°, global (360 × 181) | Adjust `qq.h` if different |
| Calendar | noleap (365-day) | Adjust `qq.h` and CTL if different |
| Time span | 1979-01-01 to 2025-02-28 | Must be a continuous daily record |

---

## Shared Configuration (`qq.h`)

Both steps use the same `qq.h`. Edit once before running either step.

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
| `iy` | Latitude index of the equator |
| `il` | Number of latitude grid points |
| `maxdaylim` | Maximum number of days (array size) |
| `nyr` | Number of years |
| `nyear` | Start year |
| `llim`, `ulim` | Latitude index range for threshold calculation (30°–90°) |
| `latres`, `lonres` | Grid resolution in degrees |
| `lowlat` | Latitude of the first grid point |
| `maxday1` | Total number of days in the input file |
| `yrtype1` | Calendar type: `1` = leap, `2` = no leap, `3` = 360-day |

---

## Step 1 — SKS Anomaly (`sks/`)

Computes the SKS anomaly and monthly threshold from the Z500 raw field.

### Edit `sks/sks.csh`

```csh
set infile1 = z500.day.19790101.20250228.noleap    # input filename (without .nc)
set InDir   = /path/to/input                        # input directory
set DatDir  = /path/to/output                       # output directory
set SrcDir  = /path/to/source                       # source code directory
```

The CTL section at the bottom of the script must match `qq.h`:

```csh
set ix     = 360
set il     = 181
set latres = 1.0
set lonres = 1.0
set lowlat = -90.0
set syear  = 1979
set maxday = 16849
```

Change `OPTIONS 365_day_calendar` in the CTL template if using a different calendar.

### Run

```bash
csh sks.csh
```

### Output

| File | Description |
|---|---|
| `SKS.z500.bin` | Z500 anomaly field (binary) → input for Step 2 |
| `cdo.monmean.ymonstd.nc` | Monthly threshold → input for Step 2 |

---

## Step 2 — Blocking Detection (`main/`)

Reads the Z500 raw field, the SKS anomaly, and the threshold from Step 1 to detect blocking events.

### Edit `main/main.csh`

```csh
set infile1a = SKS.z500                             # anomaly from Step 1
set infile1c = z500.day.19790101.20250228.noleap    # Z500 raw field
set infile1d = cdo.monmean.ymonstd                  # threshold from Step 1

set InDir  = /path/to/input
set InDir2 = /path/to/sks/output                   # Step 1 output directory
set DatDir = /path/to/output
set SrcDir = /path/to/source
```

The CTL section follows the same structure as Step 1. `nmon` is set here directly (not inherited from `qq.h`) and must equal the total number of months in the dataset.

### (Optional) Set a target region in `center.F`

Edit **line 199** of `center.F` to define a target region. Output files with the **`NP` suffix** contain results for that region only.

### Run

```bash
csh main.csh
```

### Output

| File | Description |
|---|---|
| `bdate.txt` | Blocking event info — values (e.g. date as `yyyy.mm.dd`) |
| `bday.txt` | Blocking event info — indices (e.g. date as day index `180`) |
| `relabel.nc` | Blocking field (gridded, daily, global) |
| `bdate.NP.txt` | Same as `bdate.txt`, target region only |
| `relabel.NP.nc` | Same as `relabel.nc`, target region only |

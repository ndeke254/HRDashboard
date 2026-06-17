# AttendanceDashboard

An R package for biometric analytics — daily ETL from a zkteco biometric system into a parquet data warehouse, with DuckDB query system, powering a multi-module shiny dashboard.

## Overview:

The system ingests raw punch records from a zkteco `.mdb` database, transforms them into daily attendance facts, and serves kpi cards, trend charts and employee status views via a shiny front end. DuckDB acts as the analytical engine from the parquet files forming the immutable data warehouse.

## Architecture:

```
zkteco device (.mdb)
        │
        ▼
  Run Daily ETL          
        │
        ├── departments / employees / schedules  (duckdb tables)
        │
        └── parquet data warehouse  (hive-partitioned: year / month / day)
                │
                ▼
            DuckDB queries
                │
                ▼
        shiny dashboard
          ├── kpi_overview module
          ├── employees schedule module
          ├── comparisons module
          ├── distributions module
          ├── trends module
          └── proportions modules
          
```

## ETL pipeline:

Runs end-of-day. Safe to schedule via cron or task scheduler.

1. **Guard checks:** Skips weekends and public holidays automatically.
2. **Read raw punches:** Reads `CHECKINOUT` from the `.mdb` file.
3. **Sync reference data:** Upserts any new departments or employees from the device into DuckDB. New employees receive a default mon–fri schedule.
4. **Aggregate to daily facts:** One row per employee.
5. **Compute derived metrics:** Computes binary adherence metrics against known thresholds e.g 7.5hrs, 9:00am etc.
6. **Write parquet:** Appends to a hive-partitioned lake at `parquet_root` (`year / month / day`).
7. **Clear live attendance:** removes stale rows from the `live_attendance` DuckDB table.

### key config globals:

| symbol | purpose |
|---|---|
| `mdb_path` | path to the zkteco `.mdb` file |
| `parquet_root` | root path for the parquet data warehouse |
| `tz` | timezone string (e.g. `"Africa/Nairobi"`) |
| `shift_start_mins` | shift start in minutes from midnight |
| `shift_end_mins` | shift end in minutes from midnight |
| `late_threshold` | grace period (mins) before marking late |
| `early_threshold` | grace period (mins) before marking early departure |
| `target_work_hrs` | minimum hours for full-day adherence |

## Deployment/Installation:

### windows vm (primary):

The package runs natively on windows, where the zkteco `.mdb` file is directly accessible.

Install and load the package:

```r
remotes::install_github(
  repo = "https://github.com/Actuarial-Services/AttendanceDashboard.git",
  dependencies = TRUE
)
```

Schedule `run_daily_etl()` via windows task scheduler at end of business day:

1. Open **task scheduler** → create basic task
2. Set trigger: 12:00 am, repeat mon–fri
3. Set action: **start a program**
   - program: `Rscript.exe`
   - arguments: `-e "AttendanceDashboard::run_daily_etl()"`
   - start in: `C:\path\to\AttendanceDashboard`

### docker (linux):
ensure `mdbtools` is available in the image:

```dockerfile
RUN apt-get update && apt-get install -y mdbtools
```

schedule `run_daily_etl()` via cron at end of business day:

```bash
55 17 * * 1-5 Rscript -e "AttendanceDashboard::run_daily_etl()"
```

---

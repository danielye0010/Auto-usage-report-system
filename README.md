# Automated ELN Usage Reporting Pipeline

This repository is a **sanitized engineering demonstration** of an automated reporting pipeline for an Electronic Lab Notebook (ELN) service.

It shows how usage data can move from a vendor API through cloud storage and analytics, into reproducible R-based reports and optional cloud-drive delivery. Organization-specific datasets, credentials, identifiers, report templates, names, and internal infrastructure details are intentionally not included.

## What the pipeline demonstrates

```text
ELN API
  ↓ signed API requests
Python collection / normalization
  ↓
Amazon S3
  ↓
Athena / ODBC queries
  ↓
R transformation + analytics
  ↓
RMarkdown report rendering
  ↓
Optional Google Drive delivery
```

The repository focuses on the orchestration and data-processing logic rather than publishing any production dataset.

## Components

### `collect_and_backup.py`

A Python collection stage that:

- requests ELN usage reports using HMAC-signed API calls,
- stores credentials only in environment variables,
- converts vendor CSV exports to TSV using a real CSV parser,
- uploads normalized files to a configurable S3 bucket,
- keeps organization naming configurable through `ELN_ORG_SLUG`.

### `R/`

Modular R transformations for:

- current usage preparation,
- notebook metadata preparation,
- longitudinal usage/notebook views,
- notebook-download processing,
- inactive/active lab analysis,
- summary tables and reporting helpers.

Athena view names are configurable rather than tied to a specific institution.

### `run_and_render.R`

A configuration-driven orchestration example that connects:

- Athena via ODBC,
- metadata in Google Sheets,
- the modular R transformation layer,
- optional RMarkdown templates,
- optional Google Drive report delivery.

The original production RMarkdown report templates are **not distributed** in this public repository. If `monthly_report.Rmd`, `unit_report.Rmd`, or `trimester_report.Rmd` are absent, the public orchestrator skips rendering with a warning while preserving the pipeline structure.

## Privacy and public-release boundary

This repository does **not** distribute:

- participant or user-level production data,
- real API credentials or service-account keys,
- real S3 bucket names,
- production Athena schemas/view names,
- real Google Sheet or Drive identifiers,
- internal staff names or organization-specific unit lists,
- internal report templates.

The code is intended to demonstrate the system design and automation approach without redistributing internal operational data or configuration.

## Python setup

```bash
pip install -r requirements.txt
```

Example required environment variables for the collection stage:

```bash
export LABARCHIVES_KEY_ID=...
export LABARCHIVES_ACCESS_PASSWORD=...
export LABARCHIVES_UID=...
export ELN_S3_BUCKET=...
export ELN_ORG_SLUG=organization
```

Standard AWS credentials can be supplied through the normal AWS environment/credential chain used by `boto3`.

## R configuration

The R pipeline expects its organization-specific configuration through environment variables. See `.env.example` for the public configuration surface.

Typical categories include:

- Athena credentials, region, output location, and schema,
- configurable usage/notebook/download view names,
- Google service-account key path,
- Google Sheet identifiers,
- optional Google Drive destination,
- reporting date and output directories.

Run the orchestration example with:

```bash
Rscript run_and_render.R
```

## Repository scope

This is a portfolio-safe version of a real reporting automation pattern: API acquisition, cloud backup, analytical querying, modular transformations, automated reporting, and delivery. It is not intended to recreate or expose a specific institution's production environment.

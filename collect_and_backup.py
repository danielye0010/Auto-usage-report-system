"""Collect ELN usage reports and back them up to S3.

This public example keeps credentials and organization-specific identifiers in
environment variables. It demonstrates the API-signing, report-download,
CSV-to-TSV conversion, and object-storage stages of the reporting pipeline.
"""

import base64
import csv
import datetime as dt
import hmac
import os
from hashlib import sha1
from pathlib import Path
from urllib.parse import quote
import xml.etree.ElementTree as ET

import boto3
import requests


API_ROOT = "https://api.labarchives.com/api"
OUTPUT_DIR = Path(os.getenv("ELN_OUTPUT_DIR", "data/raw"))
ORG_SLUG = os.getenv("ELN_ORG_SLUG", "organization")
AWS_REGION = os.getenv("AWS_REGION", "us-east-2")
S3_BUCKET = os.getenv("ELN_S3_BUCKET")

KEY_ID = os.getenv("LABARCHIVES_KEY_ID")
ACCESS_PASSWORD = os.getenv("LABARCHIVES_ACCESS_PASSWORD")
UID = os.getenv("LABARCHIVES_UID")

REPORT_TYPES = (
    "notebook_usage_report",
    "usage_report",
    "pdf_generation_report",
)


def require_env(name, value):
    if not value:
        raise RuntimeError(f"Missing required environment variable: {name}")
    return value


def get_epoch_expiration(session):
    response = session.get(
        f"{API_ROOT}/utilities/epoch_time",
        params={"akid": KEY_ID},
        timeout=30,
    )
    response.raise_for_status()
    root = ET.fromstring(response.text)
    return root[0].text


def signature(api_method, expires):
    raw = f"{KEY_ID}{api_method}{expires}".encode()
    digest = hmac.new(ACCESS_PASSWORD.encode(), raw, digestmod=sha1).digest()
    return quote(base64.b64encode(digest), safe="")


def download_report(session, api_method, expires, destination):
    sig = signature(api_method, expires)
    response = session.get(
        f"{API_ROOT}/site_license_tools/{api_method}",
        params={"uid": UID, "akid": KEY_ID, "expires": expires, "sig": sig},
        timeout=60,
    )
    response.raise_for_status()
    destination.write_bytes(response.content)


def csv_to_tsv(csv_path, tsv_path):
    """Convert CSV safely while preserving quoted commas and embedded fields."""
    with csv_path.open("r", encoding="utf-8-sig", newline="") as src, tsv_path.open(
        "w", encoding="utf-8", newline=""
    ) as dst:
        reader = csv.reader(src)
        writer = csv.writer(dst, delimiter="\t", lineterminator="\n")
        writer.writerows(reader)


def s3_key_for(report_type, today):
    folders = {
        "notebook_usage_report": "notebook-data",
        "usage_report": "usage-data",
        "pdf_generation_report": "download-data",
    }
    return f"{folders[report_type]}/{ORG_SLUG}_{report_type}_{today}.tsv"


def main():
    require_env("LABARCHIVES_KEY_ID", KEY_ID)
    require_env("LABARCHIVES_ACCESS_PASSWORD", ACCESS_PASSWORD)
    require_env("LABARCHIVES_UID", UID)
    require_env("ELN_S3_BUCKET", S3_BUCKET)

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    today = dt.date.today().isoformat()

    session = requests.Session()
    expires = get_epoch_expiration(session)
    s3 = boto3.client("s3", region_name=AWS_REGION)

    for report_type in REPORT_TYPES:
        csv_path = OUTPUT_DIR / f"{ORG_SLUG}_{report_type}.csv"
        tsv_path = OUTPUT_DIR / f"{ORG_SLUG}_{report_type}_{today}.tsv"

        download_report(session, report_type, expires, csv_path)
        csv_to_tsv(csv_path, tsv_path)
        s3.upload_file(str(tsv_path), S3_BUCKET, s3_key_for(report_type, today))
        print(f"Uploaded {report_type}: {tsv_path.name}")


if __name__ == "__main__":
    main()

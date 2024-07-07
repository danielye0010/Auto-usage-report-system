# Auto-usage-report-system
## Disclaimer
All privacy and sensitive data have been meticulously removed or anonymized in this project. The information and data presented here are solely for demonstration purposes. This demo aims to illustrate the project's logic and processes without compromising any confidential or personal information. Any resemblance to actual data is purely coincidental.

## Introduction

This project focuses on automating the generation and processing of monthly and trimester reports for the usage of Electronic Lab Notebooks (ELN). The goal is to provide detailed insights into how ELNs are utilized across different departments and units within an organization. By leveraging data from multiple sources, the project aims to facilitate better understanding and analysis of ELN usage patterns.

## Objectives

- **Automate Report Generation**: Eliminate manual efforts in generating monthly and trimester ELN usage reports.
- **Data Integration**: Combine metadata and usage data from various sources to create comprehensive reports.
- **Detailed Insights**: Provide detailed usage metrics and trends to help departments understand and optimize their ELN usage.

## Pipeline

### Data Collection and Preprocessing

The project includes a Python script that automates the download and preprocessing of data:

1. **Data Download**:
    - **API Interaction**: The script interacts with the LabArchives API to download reports on notebook usage, overall usage, and PDF generation.
    - **Signature Generation**: It generates secure API request signatures using HMAC and SHA1 for authentication.

2. **File Processing**:
    - **Format Conversion**: Converts CSV files to TSV format for consistency.
    - **File Renaming**: Renames files with the current date to maintain version control.

3. **AWS S3 Upload**:
    - **Upload to S3**: The processed files are uploaded to AWS S3 for storage and further processing.

### Data Processing and Report Generation

The project then uses R scripts to further process the data and generate reports:

1. **Data Cleaning and Integration**:
    - **Read Data**: Load usage data and metadata from Google Sheets and other sources.
    - **Clean Data**: Filter and clean the data to ensure accuracy and consistency.

2. **Usage Analysis**:
    - **Current Month's Data**: Set up and analyze the current month's usage data.
    - **Trends Over Time**: Analyze data over time to identify trends and patterns in ELN usage.

3. **Report Generation**:
    - **Generate Reports**: Create detailed PDF reports for overall monthly and trimester usage.
    - **Unit-Specific Reports**: Generate reports tailored to different departments and units.
    - **Automated Rendering**: Use RMarkdown to automate the rendering of reports, ensuring consistency and accuracy.

### File Transfer and Storage

The final step involves transferring the generated reports to a designated Google Drive folder:

1. **Organize Reports**: Create folders for each report period on Google Drive.
2. **Upload Reports**: Automatically upload the generated reports to the appropriate folders for easy access and sharing.

## Achievements

- **Efficiency Improvement**: Significant reduction in time and effort required to generate and distribute ELN usage reports.
- **Data-Driven Decisions**: Enhanced ability for departments to make informed decisions based on detailed usage insights.
- **Scalability**: The automated pipeline is designed to handle an increasing amount of data and reports as the organization's needs grow.
- **Reliability**: Automated processes reduce the risk of human error, ensuring accurate and consistent reporting.

## Contribution and maintenance  
UW-Madison Research Cyberinfrastructure - ELN Service team

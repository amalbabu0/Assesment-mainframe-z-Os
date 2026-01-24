# Student Record Processing – IBM Mainframe Mini Case Study

Overview
This project is an IBM Mainframe mini case study that demonstrates end-to-end
batch processing using ISPF, JCL, SORT, VSAM KSDS, and COBOL.

The application processes student records from sequential files, validates
data using COBOL, performs calculations using a subprogram, and stores
processed results in a VSAM KSDS file.

Technologies Used
- ISPF
- JCL
- DFSORT
- COBOL (Main Program + Subprogram)
- VSAM (KSDS)
- z/OS

Input Data Details
Input files are sequential datasets with LRECL 80.

Fields:
- Student_Id   : 9(5)
- First_Name   : X(10)
- Last_Name    : X(10)
- Marks        : 9(2).9(2)

Special characters (@, $, %, &) may appear in First_Name.

Processing Flow
STEP 1: ISPF
- Create two PS datasets (PS1 and PS2)
- Header record included
- Records split between PS1 and PS2

STEP 2: JCL
- Allocate two VSAM KSDS datasets
  - Reference KSDS
  - Output KSDS

STEP 3: JCL (SORT)
- Remove header records
- Merge PS1 and PS2 into PS3
- Increment Student_Id by 1
- Sort records in ascending order
- Load data into Reference KSDS

STEP 4: COBOL
- Validate input fields
- Skip invalid records
- Replace special characters in First_Name
- Generate new fields
- Calculate percentage using a subprogram
- Write valid records to Output KSDS

Validations Implemented
- Student_Id must be numeric
- First_Name must not be blank
- Last_Name must be alphabetic
- Marks must be numeric with valid decimal format

Business Logic
Percentage = (Marks / 50) * 100
Rounded to nearest integer

Result:
- ≥ 70 → "Congratulations!!!"
- < 70 → "Better luck next time!!!"

Output File Layout
Student_Id_new  X(06)
New_Name        X(15)
Marks           9(2).9(2)
Percentage      9(02)
Result          X(30)

One space separator between all fields

How to Run
1. Execute VSAM allocation JCL
2. Run SORT JCL to create PS3
3. Compile COBOL main and subprogram
4. Run RUNJCL to generate output KSDS

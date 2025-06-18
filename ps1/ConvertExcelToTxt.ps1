param(
    [Parameter(Mandatory = $true)]
    [string]$InputFile="C:\Users\uakaz\ps1\1_Custom_sec_14April2025_10226_PM.xls",

    [Parameter(Mandatory = $true)]
    [string]$OutputFile="C:\Users\uakaz\ps1\1_Custom_sec_14April2025_10226_PM.xls.txt",

    [Parameter(Mandatory = $false)]
    [int]$SkipRows = 0
)

$excel = New-Object -ComObject Excel.Application
$excel.Visible = $false
$workbook = $excel.Workbooks.Open($InputFile)
$worksheet = $workbook.Worksheets.Item(1)

$usedRange = $worksheet.UsedRange
$usedRange.NumberFormat = "General"

$range = $worksheet.Range("A:A")
$range.NumberFormat = "yyyy-mm-dd"

$range = $worksheet.Range("B:B")
$range.NumberFormat = "hh:mm:ss"

$range = $worksheet.Range("1:$SkipRows")
$range.EntireRow.Delete()

# Сохраняем как текстовый файл (Юникод)
$xlUnicodeText = 42
$workbook.SaveAs($OutputFile, $xlUnicodeText)

$workbook.Close($false)
$excel.Quit()
[System.Runtime.Interopservices.Marshal]::ReleaseComObject($excel) | Out-Null

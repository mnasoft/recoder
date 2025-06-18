param(
    [Parameter(Mandatory = $true)]
    [string]$InputFile ,

    [Parameter(Mandatory = $true)]
    [string]$OutputFile ,

    [Parameter(Mandatory = $false)]
    [int]$SkipRows = 0
)

$excel = New-Object -ComObject Excel.Application
$excel.Visible = $true
$workbook = $excel.Workbooks.Open($InputFile)
$worksheet = $workbook.Worksheets.Item(1)

$worksheet.Cells.NumberFormat = "��������"
$worksheet.Columns.Item("A").NumberFormat = "��.��.����"
$worksheet.Columns.Item("B").NumberFormat = "��:��:��"

$worksheet.Range("1:$SkipRows").EntireRow.Delete()

# ��������� ��� ��������� ���� (������)
$xlUnicodeText = 42
$workbook.SaveAs($OutputFile, $xlUnicodeText)

$workbook.Close($false)
$excel.Quit()
[System.Runtime.Interopservices.Marshal]::ReleaseComObject($excel) | Out-Null

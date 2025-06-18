powershell -ExecutionPolicy Bypass -File ConvertExcelToTxt.ps1 -InputFile "C:\вход.xlsx" -OutputFile "C:\выход.txt" -SkipRows 15



$start = $ws.Range("A15")
$end = $start.End(-4167) # -4167 — xlUp (Ctrl + ↑)
Write-Host "Новая ячейка: $($end.Address)"
Write-Host "Значение: $($end.Value())"

1 — xlToLeft (Ctrl + ←)

2 — xlToRight (Ctrl + →)

3 — xlDown (Ctrl + ↓)

-4167 — xlUp (Ctrl + ↑)


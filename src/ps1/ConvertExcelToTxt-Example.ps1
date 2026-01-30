$Script = "D:\home\_namatv\PRG\msys64\home\namatv\quicklisp\local-projects\clisp\recoder\ps1\ConvertExcelToTxt.ps1"
$InputFile = "D:\home\_namatv\PRG\msys64\home\namatv\quicklisp\local-projects\clisp\recoder\trd\1_Custom_sec_14April2025_10226_PM.xls"
$OutputFile = "D:\home\_namatv\PRG\msys64\home\namatv\quicklisp\local-projects\clisp\recoder\trd\1_Custom_sec_14April2025_10226_PM.xls.txt"
$SkipRows = 15

powershell -ExecutionPolicy Bypass -File $Script -InputFile  $InputFile -OutputFile $OutputFile -SkipRows $SkipRows

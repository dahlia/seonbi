[CmdletBinding()] param ()

Set-Variable ObjdumpPath -Option Constant -Value (stack path --compiler-bin `
  | Split-Path -Parent `
  | Join-Path -ChildPath "mingw" -AdditionalChildPath "bin", "objdump.exe")

function Get-RequiredDlls {
  [OutputType([System.IO.FileInfo[]])]
  param (
    [Parameter(Mandatory)]
    [System.IO.FileInfo]$ObjectPath,
    [Parameter(Mandatory)]
    [System.IO.FileInfo]$LibraryPath
  )
  $dlls = & $ObjdumpPath -p $ObjectPath `
    | Select-String "^`tDLL Name: (.*?`.[Dd][Ll]{2})$" -CaseSensitive `
    | ForEach-Object { $_.Matches.Groups[1].Value }
  $dllsToBundle = Get-ChildItem -Filter *.dll -Recurse $LibraryPath `
    | Where-Object { $dlls -contains $_.Name }
  if ($null -eq $dllsToBundle -or $dllsToBundle.Length -lt 1) {
    return @()
  } elseif ($dllsToBundle.GetType() -eq [System.IO.FileInfo]) {
    $dllsToBundle = @($dllsToBundle)
  }
  $dependencies = @()
  foreach ($dll in $dllsToBundle) {
    $dependencies += Get-RequiredDlls $dll $LibraryPath
  }
  if ($dependencies.Length -gt 0) {
    $dllsToBundle += $dependencies
  }
  $dllsToBundle = $dllsToBundle | Select-Object -Unique
  if ($dllsToBundle.GetType() -eq [System.IO.FileInfo]) {
    return @($dllsToBundle)
  }
  return $dllsToBundle
}

$localBinDir = stack path --local-install-root | Join-Path -ChildPath "bin"
$objectPaths = Get-ChildItem -Filter *.exe $localBinDir
$libraryPath = stack path --compiler-bin | Split-Path -Parent

foreach ($obj in $objectPaths) {
  Get-RequiredDlls $obj $libraryPath | ForEach-Object {
    Write-Verbose $_
    Copy-Item $_ $localBinDir
  }
}

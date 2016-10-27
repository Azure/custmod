$SourceFolder    = "D:\Temp\git\custmod\AssociationRules"
$DestinationFile = "D:\Temp\git\custmod\AssociationRules\ar.zip"
$Compression     = "Optimal"  # Optimal, Fastest, NoCompression

if (Test-Path $DestinationFile)
{
    Remove-Item $DestinationFile
}

function Zip-Directory {
    Param(
      [Parameter(Mandatory=$True)][string]$DestinationFileName,
      [Parameter(Mandatory=$True)][string]$SourceDirectory,
      [Parameter(Mandatory=$False)][string]$CompressionLevel = "Optimal",
      [Parameter(Mandatory=$False)][switch]$IncludeParentDir
    )
    Add-Type -AssemblyName System.IO.Compression.FileSystem
    $CompressionLevel    = [System.IO.Compression.CompressionLevel]::$CompressionLevel  
    [System.IO.Compression.ZipFile]::CreateFromDirectory($SourceDirectory, $DestinationFileName, $CompressionLevel, $IncludeParentDir)
}

Zip-Directory -DestinationFileName $DestinationFile -SourceDirectory $SourceFolder -CompressionLevel $Compression #-IncludeParentDir #Optional parameter

Import-Module .\AzureMLPS.dll
New-AmlCustomModule -ConfigFile .\config_sichuan.json -CustomModuleZipFileName $DestinationFile
#New-AmlCustomModule -ConfigFile .\config_sichuan.json -CustomModuleZipFileName 'C:\Users\haining\Desktop\Temp\custom modules\AddManyColumns.zip' 

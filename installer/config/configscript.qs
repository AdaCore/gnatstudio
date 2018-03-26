
function Controller()
{
    if (systemInfo.productType == "windows")
    {
        installer.setValue("TargetDir", "C:\\GPS\\@ProductVersion@");
    }
}


function Component()
{
    component.loaded.connect(this, addRegisterFileExtensionsPage);
}

function addRegisterFileExtensionsPage()
{
    if (installer.isInstaller() && installer.value("os") == "win")
    {
        //  Add checkbox to register file extensions during installation.
        installer.addWizardPageItem
           (component,
            "RegisterFileExtensionsForm",
            QInstaller.TargetDirectory);
    }
}

Component.prototype.createOperations = function()
{
    //  Call default implementation to install files.
    component.createOperations();

    if (component.userInterface("RegisterFileExtensionsForm").RegisterFileExtensionsCheckBox.checked && installer.value("os") == "win")
    {
        component.addOperation("RegisterFileType", "ada", "@TargetDir@\\bin\\gps.exe %1", "Ada file", "text/plain", "@TargetDir@\\gnaticons.dll,17");
        component.addOperation("RegisterFileType", "ads", "@TargetDir@\\bin\\gps.exe %1", "Ada spec file", "text/plain", "@TargetDir@\\gnaticons.dll,16");
        component.addOperation("RegisterFileType", "adb", "@TargetDir@\\bin\\gps.exe %1", "Ada body file", "text/plain", "@TargetDir@\\gnaticons.dll,15");
        component.addOperation("RegisterFileType", "gpr", "@TargetDir@\\bin\\gps.exe -P%1", "GNAT project file", "text/plain", "@TargetDir@\\gnaticons.dll,14");
    }
}

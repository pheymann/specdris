$project = "specdris.ipkg"
$testpkg = "test.ipkg"

function SpecDris-Clean {
    echo "clean project"
    idris --clean $project
    
    cd test; 
    idris --clean $testpkg; 
    
    cd ..
}

function SpecDris-Test {
    echo "test project"
    
    cd test
    idris --testpkg $testpkg
    
    If ( $? ) { echo "SUCCESSFUL" }
    Else { echo "FAILURE"; exit 1; }

    cd ..
}

function SpecDris-BuildNoTest() {
    echo "build project"
    idris --build $project
}

function SpecDris-Build() {
    SpecDris-BuildNoTest
    SpecDris-Test
}

function SpecDris-Install() {
    echo "install project"
    SpecDris-Clean
    SpecDris-Test

    If ( $? ) { idris --install $project }
    Else { echo "couldn't install project > something went wrong"; exit 1 }
}

function SpecDris-Help() {
    echo "usage: 'Project.ps1 [command]'"
    echo "  --clean: cleans the project"
    echo "  --build: builds the code and runs all tests"
    echo "  --buildNoTest: builds the code only" 
    echo "  --test: runs all tests"
    echo "  --install: installs the project"
    echo "  --help: prints the help"
}

switch ($args[0]) {
    "--clean" { SpecDris-Clean }
    "--build" { SpecDris-Build }
    "--buildNoTest" { SpecDris-BuildNoTest }
    "--test" { SpecDris-Test }
    "--install" { SpecDris-Install }
    "--help" { SpecDris-Help }
    default { echo "unknown command"; SpecDris-Help }
}
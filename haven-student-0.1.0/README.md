# How to use haven student for validation
## Installation
You should have installed:
- Java 11 
- Stack 2.9.x (tested with 2.9.1)

### How to install Java
Oracle has made it more difficult to use Java from Oracle by making changes to the license and requiring a login.
Therefore, I give you a few other sites where you can download Java.
- [Liberica JDK](https://bell-sw.com/pages/downloads/#/java-11-lts)
  1. download the executable for your plattform
    - You can change the package to `Standard JRE` if you don't want to program Java in the future.
  2.install the executable
- [IBM JDK (Advanced)](https://developer.ibm.com/languages/java/semeru-runtimes/downloads/)
  1. Select `Semeru Runtime Open Edition` at first selection box
  2. Select `Java 11(LTS)` at second selection box
  3.Select your operating system in third selection box
  4.Download the latest executable 
    - You can download `JRE` if you don't want to program Java in the future.
    - For Beginners:
      - Windows: you should download the `msi`.
      - MacOS: you should download the `pkg`
  - install the executable
- [OpenJDK](https://jdk.java.net/java-se-ri/11) (Windows and Linux only)
  - If you have linux: you can install it also with your package manager
  1. download the executable for your plattform
  2. install the executable

## Usage
- run `java -jar haven-student-0.1.0.jar validate -f ./swtpp-ha-cabal.zip`
  - `validate` stands for validation only
  - `-f /path/to/file.zip` is the path to your submission zip file
  - If you have stack not normally installed you can use `-s /path/to/stack` to tell the program where your stack is
  - if you have problems you can add additional `-v` to enable verbose mode

## How to report bugs
We rely on your feedback. So if you find bugs you should be able to report them [here](https://git.tu-berlin.de/haven/haven/-/issues).
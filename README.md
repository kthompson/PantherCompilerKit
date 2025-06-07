# Panther Compiler Kit

This is the **Panther Compiler Kit**.

## Setup

To install dependencies for this sbt project, follow these steps:

1. **Install Java**  
   Make sure you have Java (JDK 8 or higher) installed. You can download it from [AdoptOpenJDK](https://adoptopenjdk.net/) or [Oracle](https://www.oracle.com/java/technologies/javase-downloads.html).

2. **Install sbt**  
   Download and install sbt from the [official website](https://www.scala-sbt.org/download.html).

3. **Fetch Dependencies**  
   Open a terminal in the project directory and run:
   ```sh
   sbt update
   ```
   This will download and install all required dependencies.

You're now ready to build and run the project!


## Getting Started

### Building

```sh
sbt compile
```

### Running Tests

```sh
sbt test/run
```
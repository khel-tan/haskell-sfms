# Simple File Management System in Haskell

## Short description
A virtual file management system in the spirit of UNIX/Linux written in Haskell

## Table of Contents
<!-- - [**If you are a prospective employer, please go here to see a summary of notable skills that this project showcases**](#for-potential-employers) -->
- [Currently implemented features](#features)
- [Intended features yet to be implemented](#intended-features)
- [What a big project! How do I navigate this?](#how-to-navigate-the-project)
<!-- - [Why did I do some of the things that I did?](#why-did-I-do-it?) -->
- [This looks cool! How do I run it?](#how-to-run)
<!-- - [Project Roadmap](#roadmap) -->

## Features
- Commands
  - cd : Change directory
  - search : Search for files/directories with a given name or a substring in its name
  - ls : List contents of current directory
  - tree : Print a pretty tree of the directory structure with the current directory as the root
  - mkdir : Create a new directory
  - create : Create a new file
  - open : Open file
  - close : Close file
  - cat : Print the contents of a file
  - update : Update an open file
  - rename : Rename the currently focused directory/file
  - print : Print the name and content of the currently open file
  - rm : Remove the specified directory/file
  - ? : Exposition about the available commands and their details

## Intended Features
- Search by substrings in names or content
- Unit Testing
- Persistent storage

## How To Run
- Firstly, clone the project
- Run `stack build`
- Run `stack exec haskell-sfms-exe` and enjoy!
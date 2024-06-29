# Simple File Management System in Haskell

## Short description
A virtual file management system in the spirit of UNIX/Linux written in Haskell

## Table of Contents
- [**If you are a prospective employer, please go here to see a summary of notable skills that this project showcases**](#for-potential-employers)
- [Currently implemented features](#features)
- [Intended features yet to be implemented](#intended-features)
- [What a big project! How do I navigate this?](#how-to-navigate-the-project)
  + [Overview](#overview)
  + [Decoding instructions](#decoding-instructions)
  + [Opcodes](#opcodes)
- [Why did I do some of the things that I did?](#why-did-I-do-it?)
- [This looks cool! How do I run it?](#how-to-run)
- [Project Roadmap](#roadmap)

## Features
- Basic commands such as cd, ls, mkdir
    - cd works on absolute and relative paths
- Basic search by name within the current directory
- CRUD commands on files
  - create
  - cat (read)
  - update
  - delete

## Intended Features
- Search by substrings in names or content
- Unit Testing
- Persistent storage

## How To Run
- Firstly, clone the project
- Run `stack build`
- Run `stack exec haskell-sfms-exe` and enjoy!
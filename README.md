# js-analyzer

## Tasks
- [x] Extract metadata on repositories: name, number of stars, commits, contributors
  - [x] name
  - [x] number for stars
  - [x] number of commits
  - [x] number of forks
  - [ ] number of contributors
- [x] Extract code for latest default branch
- [x] Extract data on direct dependencies: Ramda, Lodash, Underscore.js, Rambda, Folktale, functional.js, Immutable.js, 
      fluture, monet.js, Sanctuary, Fantasy-land
- [x] Count for each project file:
  - [x] get content of repo as AST
  - [x] Lines of codes
  - [ ] Some other metric of code complexity?
  - [x] Number of while/for/do-while loops
  - [x] Number of map/filter/reduce/forEach calls
- [x] Repeat processes for project mid-life? (What's the project midlife, anyway?)

## Setup

### Requirements

requires Node 8 or later to be available on path.

### Environment

Expects `GITHUB_TOKEN` to contain a valid token for github. This is not a requirement, but github might impose lower rate limit on requests without token.

### Build Esprima Wrapper

    cd js-parser
    npm install

### Building

    stack build

## Running

### Using Stack

Run the following to get all command line options:

    stack run -- --help
    
The code can search github for repositories, or get a list of github repositories in the following format:

    owner/repoName

separated by new lines or commas.
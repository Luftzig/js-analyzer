# js-analyzer

## Tasks
- [ ] Extract metadata on repositories: name, number of stars, commits, contributors
- [x] Extract code for latest default branch
- [ ] Extract data on direct dependencies: Ramda, Lodash, Underscore.js, Rambda, Folktale, functional.js, Immutable.js, 
      fluture, monet.js, Sanctuary, Fantasy-land
- [ ] Count for each project file:
  - Lines of codes
  - Some other metric of code complexity?
  - Number of while/for/do-while loops
  - Number of map/filter/reduce/forEach calls
- [ ] Repeat processes for project mid-life? (What's the project midlife, anyway?)

## Setup

### Environment

Expects `GITHUB_TOKEN` to contain a valid token for github. This is not a requirement, but github might impose lower rate limit on requests without token.
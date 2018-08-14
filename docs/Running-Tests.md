This document was created in order to provide one central location to
describe all use cases of build and test automation. Most or all of
these should be run once, whenever making a sweeping change such as how
the back end or front end build automation works.

# Haskell interpreter load / run

  - duplicate from "How to run laptop"

# Build / Package

### Running locally

Get latest script and run script

    wget -O - https://rawgit.com/databrary/databrary-incubator/master/databrary-devops/build-from-scratch.sh | bash

### Running on build server

Get latest script and run script (must have sudo permissions on build
server).

  - Replace \<USERID\> with your netid
  - Be prepared to enter credentials two times.
    1.  When connecting to devdatabrary2
    2.  When using sudo to become ka988


    ssh -t @devdatabrary2.home.nyu.edu 'sudo su - ka988 -c "wget -O - https://rawgit.com/databrary/databrary-incubator/master/databrary-devops/build-from-scratch.sh | bash"'



# Back end unit tests

    # possibly clean local db
    # rm -rf databrary-nix-db
    # run init-psql command to create new db
    make cabal-test

# Front end unit tests

  - Not started yet, start soon

# Back end integration tests

  - Deploy branch to apitest1 using "how to run dev v3"
  - Go to this url in browser or with curl
    "<https://api.runscope.com/radar/1e8e2c34-3dab-40c6-a3bb-6d55ea2f9a87/trigger?runscope_environment=fe172402-88b1-4363-88ab-f61f319c0914>"
    to start test run
  - Login to runscope, go to "Route Test Suite" → "Dev Route Tests" to
    observe results
  - If you get "red"/"failed", contact Minyong
  - If you get "green"/"passed", then proceed

# Hot reload

  - reload once:
  - continuously reload:

# Front end integration tests

  - Not started yet

# Functional (full stack) tests

  - See separate page and video
      - interactively on windows or OS X -
        <https://drive.google.com/drive/u/1/folders/16BEKtNEDCYF1mXeLPc0dT3Qchrm3ldOO>

      - command line on ubuntu - ...

# Generate Haddocks

  - ....

# Generate Backend Unit Test Coverage Report

  - ....

# Build / Package / Deploy / Run

  - model after build → stage, but create new dedicated AWS servers

# Monitor Production Back End

  - uptime robot - configured for automatic run against prod currently;
    emails parties when down
  - runscope - access read only API endpoints on a schedule
  - NYU nagios - ...

-----



Notes

  - best switch user approach -
    <https://unix.stackexchange.com/questions/48626/change-user-and-load-entire-environment-in-shell-script>

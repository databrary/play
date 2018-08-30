# Back end unit tests

    # possibly clean local db
    # rm -rf databrary-nix-db
    # run init-psql command to create new db
    make cabal-test

# Back end integration tests

  - Deploy branch to apitest1 using "how to run dev v3"
  - Go to this url in browser or with curl
    "<https://api.runscope.com/radar/1e8e2c34-3dab-40c6-a3bb-6d55ea2f9a87/trigger?runscope_environment=fe172402-88b1-4363-88ab-f61f319c0914>"
    to start test run
  - Login to runscope, go to "Route Test Suite" → "Dev Route Tests" to
    observe results
  - If you get "red"/"failed", contact Minyong
  - If you get "green"/"passed", then proceed

# Functional (full stack) tests

  - See separate page and video
      - interactively on windows or OS X -
        <https://drive.google.com/drive/u/1/folders/16BEKtNEDCYF1mXeLPc0dT3Qchrm3ldOO>

      - command line on ubuntu - ...

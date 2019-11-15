# Functional Flows Calculator

[![Build Status](https://travis-ci.org/leogoesger/func-flow.svg?branch=master)](https://travis-ci.org/leogoesger/func-flow)

## About

This project uses [Python3](https://www.python.org/)

## Getting Started in 4 simple steps (Mac OS)

1. Install [Python3](https://www.python.org/downloads/), [Git](https://git-scm.com/download/) and a [text editor](https://www.sublimetext.com/3) of your choice.
2. Clone your project in [Terminal](http://www.informit.com/blogs/blog.aspx?uk=The-10-Most-Important-Linux-Commands)

   ```
   git clone https://github.com/leogoesger/func-flow.git
   cd func-flow/
   ```

3. Create and activate virtualenv

   ```
   python3 -m venv my-virtualenv
   source my-virtualenv/bin/activate
   ```

4. Install dependencies
   ```
   pip install -r requirements.txt
   ```

## Getting Started in 5 steps (Windows)

1. Install [Python3](https://www.python.org/downloads/), [Git](https://git-scm.com/download/win) and a [text editor](https://www.sublimetext.com/3) of your choice.
2. Add Python to [System Path](https://www.pythoncentral.io/add-python-to-path-python-is-not-recognized-as-an-internal-or-external-command/)

   - Locate `Python3` from your local computer. Usually located in the following folder:

     ```
     C:\python3
     ```

     or

     ```
     C:\Users\your-name\AppData\Local\Programs\Python\Python36-32
     ```

   - Follow this [link](https://www.pythoncentral.io/add-python-to-path-python-is-not-recognized-as-an-internal-or-external-command/) from step 2 to the end.
   - Go into Command Prompt by typing `cmd` in search bar, and type `python`. You should see the following:

     ```
     Python 3.6.4 (v3.6.4:d48ecebad5, Dec 18 2017, 21:07:28)
     [GCC 4.2.1 (Apple Inc. build 5666) (dot 3)] on darwin
     Type "help", "copyright", "credits" or "license" for more information.
     >>>
     ```

   - Type `exit()` to exit the python shell.

3. Clone your project in [Command Prompt](http://www.informit.com/blogs/blog.aspx?uk=The-10-Most-Important-Linux-Commands)

   ```
   git clone https://github.com/larflows/func-flow.git
   cd func-flow
   ```

4. Create and activate virtualenv

   ```
   python -m venv my-virtualenv
   my-virtualenv\Scripts\activate
   ```

5. Install dependencies

   ```
   pip install -r requirements.txt
   ```

## Run Script

1. In project directory

   ```
   python main.py
   ```

## Using referee (R interface)

The R interface allows uploading from data frames and reading out results.  For now, the variable EFF_DIR in referee.R must be
set to the top-level func-flow directory.  Also, the PYTHON_PATH variable must be set to the location of Python
3 (the script may not work with Python 2).
Otherwise, the script should work out of the box.

Multi-gage flow data can be uploaded through `upload_gagedata`, which requires a data frame with the columns `gage` (character),
`data` (character - mm/dd/yyyy), and `flow` (cfs).  The data for a single gage can be uploaded by using `write_input_df` and then
`upload_files`, and the data can be put into the appropriate format with `make_input_df`.

Results can be read out into data frames (specifically, tibbles) through `get_annual_flow_result`.  `get_annual_flow_matrix` and
`get_drh` are also available, but do not process the data into a user-friendly format, unlike `get_annual_flow_result` (as
the annual flow result data appears to be the most relevant).

The function `process_gages` will take a multi-gage data frame and return a list of all of the flow result data frames without
any other user intervention.  It will also work with a single gage with or without a gage column.  It is unlikely that most users will need to use any of the lower-level functions described above.  If the data is not in the correct format (gage, date as character mm/dd/yyyy, flow as cfs), then
the function `format_and_process` should be used instead.  The options are described in the comments in
`format_input_df`.

Example data can be generated by `example_gagedata`.  Thus, running `process_gages(example_gagedata())` will demonstrate the
full functionality of the script as far as annual flow results are concerned.

## Development

1. For older to newer python upgrade

   ```
   python server, when not able to create python env
   python -m venv test --without-pip
   source test/bin/activate
   curl https://bootstrap.pypa.io/get-pip.py | python
   ```

## Error and Bug

Use [Trello](https://trello.com/funcflow) to keep upload error message, a screen shot, and raw data file used

## Options

[iTerm](https://www.iterm2.com/): iTerm2 is a replacement for Terminal

```
find . -name '*.rdb' -exec sh -c 'mv "$0" "${0%.rdb}.csv"' {} \;

  pip freeze > requirements.txt
```

## Flask server

To start flask server, first init virtualenv and then install all dependencies. `flask run`

## License

Copyright (c) 2018

Licensed under the [MIT license](LICENSE).

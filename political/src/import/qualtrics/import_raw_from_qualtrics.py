import sys
from selenium import webdriver
import pandas as pd
import requests
from bs4 import BeautifulSoup
from utils import username 
from utils import password 
import time

LOGIN_URL = 'https://login.qualtrics.com/login?lang=en'
OUT_PATH_ROOT = '../../../data/raw/'


def get_survey_webpage(survey_id):
    '''
    Get the source HTML for the webpage containing the table with
    the raw survey data from the survey with ID survey_id.

    Inputs:
      survey_id (str): identification string of the survey (you
        can find this on Qualtrics by inspecting the survey element
        of the table).

    Returns:
      html: HTML source object 
    '''
    session = webdriver.Firefox(
        executable_path='/mnt/c/ProgramData/geckodriver/geckodriver.exe')
    session.get(LOGIN_URL)
    # Include sleep calls so the page has time to load before attempting
    # to manipulate it further
    time.sleep(15)
    session.find_element_by_id('UserName').send_keys(username)
    session.find_element_by_id('UserPassword').send_keys(password)
    session.find_element_by_id('loginButton').click()
    time.sleep(15)
    session.find_element_by_id(survey_id).click()
    time.sleep(15)
    session.find_element_by_xpath("//span[text()='Data & Analysis']").click()
    html = session.page_source
    return html


def scrape_raw_data(html):
    '''
    Takes the HTML object gleaned from the Qualtrics scraping
    process and pull out the data in a row-wise fashion.

    Inputs:
      html: the html object returned by get_survey_webpage
    returns:
      translated_rows (list of lists): raw survey data in
        list-of-list form where each row is one observation
    '''
    soup = BeautifulSoup(html, 'html.parser')
    table = soup.find('table')
    rows = table.findAll('tr')

    row_data = []
    # Ignore the table header
    for row in rows[1:]:
        # Manually adjust the range based on how many
        # questions you have in the survey (make sure
        # you're capturing every cell of data) - this
        # will be updated based on how you've set your
        # survey viewing preferences in Qualtrics
        row_data.append(row.findAll('td')[1:21])

    translated_rows = []
    for row in row_data:
        row_to_add = []
        # For each individual cell in each row, pull out the text
        # that represents the response
        for cell in row:
            try:
                data = cell.find('span', {'class':'ng-binding'}).text
            except AttributeError:
                data = ''
            row_to_add.append(data)
        translated_rows.append(row_to_add)

    return translated_rows


def convert_and_export(rows, survey_id, extension1, extension2):
    '''
    Convert the data to a dataframe and write it out to the 
    specified location.

    Inputs:
      rows (list of lists): the raw survey data returned by the 
        scrape_raw_data function
      extension (str): end of the file path at which to save
        the processed file.
    '''
    # Update the columns call with the appropriate headers as the
    # survey questions change
    metadata_cols = ['survey_id', 'response_id', 'start_date',
                     'duration', 'lat', 'long', 'random_id']
    response_cols = ['survey_id', 'response_id', 'registered', 'propensity',
                     'pres', 'gov', 'sen', 'party_reg', 'age', 'gender', 
                     'hisp', 'race', 'educ', 'pres16', 'house18', 'county']

    out_df = pd.DataFrame(rows,
        columns = ['response_id', 'start_date', 'duration', 'long', 'lat', 'registered', 
                   'propensity', 'pres', 'gov', 'sen', 'party_reg', 'age', 'gender', 
                   'hisp', 'race', 'educ', 'pres16', 'house18', 'county', 'random_id'])
    
    # Define survey_id field and move it to the front of the df
    out_df.loc[:, 'survey_id'] = survey_id
    metadata_df = out_df[metadata_cols]
    response_df = out_df[response_cols]

    out_path_metadata = OUT_PATH_ROOT + extension1
    out_path_response = OUT_PATH_ROOT + extension2

    metadata_df.to_csv(out_path_metadata, index=False)
    response_df.to_csv(out_path_response, index=False)


def go(args):
    '''
    Chain together functions to run data scraping process end-to-end.
    Called from the command line using the passed-in arguments for
    survey ID and output save path extension.
    '''
    usage = ("usage: python3 import_raw_from_qualtrics.py <survey_id> " \
             "<metadata_path_extension> <response_path_extension>")
    if len(args) != 4:
        print(usage)
        sys.exit(1)

    html = get_survey_webpage(args[1])
    raw_data = scrape_raw_data(html)
    convert_and_export(raw_data, args[1], args[2], args[3])


if __name__ == "__main__":
    go(sys.argv)

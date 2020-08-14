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
        # you're capturing every cell of data)
        row_data.append(row.findAll('td')[1:15])

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


def convert_and_export(rows, extension):
    '''
    Convert the data to a dataframe and write it out to the 
    specified location.

    Inputs:
      rows (list of lists): the raw survey data returned by the 
        scrape_raw_data function
      extension (str): end of the file path at which to save
        the processed file.
    '''
    out_df = pd.DataFrame(rows,
        columns = ['registered','propensity','pres','gov','sen','party_reg',
                   'gender','age','hisp','race','educ','pres16','house18','county'])

    out_path = OUT_PATH_ROOT + extension
    out_df.to_csv(out_path, index=False)


def go(args):
    '''
    Chain together functions to run data scraping process end-to-end.
    Called from the command line using the passed-in arguments for
    survey ID and output save path extension.
    '''
    usage = ("usage: python3 import_raw_from_qualtrics.py <survey_id> <out_path_extension>")
    if len(args) != 3:
        print(usage)
        sys.exit(1)

    html = get_survey_webpage(args[1])
    raw_data = scrape_raw_data(html)
    convert_and_export(raw_data, args[2])


if __name__ == "__main__":
    go(sys.argv)

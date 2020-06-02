"""This script take amsderdam grom wikipedia and finds the longest sentence in the description"""

import wikipedia as wiki

try: 
    city = wiki.page("Amsderdam")
    desc = city.summary

    max_len = 0
    sen = ""
    for sent in desc.split("."):
        if len(sent.split(" ")) > max_len:
            max_len = len(sent.split(" "))
            sen = sent


    print("The longest sentence in the description of Amsderdam is ", sen)
    print("And its length is ", max_len)

except wikipedia.exceptions.WikipediaExecption:
    print("page cannot find")

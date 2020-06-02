"""The First part of this script maps the companies to the cities where they exists. Data located in data/cities.json json file.
The second part of this script find good cities. City is good if it has more then 3 bars in it"""
import json

with open("/home/grigor/Applied-Statistics-and-Data-Science/python/data/cities.json", "r") as f:
    data = json.load(f)


comp_to_cities = {}
for cit in data:
    for company in cit["company"]:
        comp_name = company["name"]
        if comp_to_cities.get(comp_name) != None:
            comp_to_cities[comp_name].append(cit["city"])
        else:
            comp_to_cities[comp_name] = [cit["city"]]
print("mapping company to cities  ")
print(comp_to_cities)

good_cities = []
for cit in data:
    if len(cit["bars"]) > 3:
        good_cities.append(cit["city"])

print("good cities are ")
print(good_cities)


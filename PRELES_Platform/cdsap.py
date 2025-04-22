def fetch_data():
  
  return 1


import cdsapi
    
client = cdsapi.Client(
  url: "https://cds.climate.copernicus.eu/api""
  key: "f1acc66d-4ff2-4bf4-bf12-5daa0b215933""
)

dataset = "reanalysis-era5-pressure-levels"
request = {
    "product_type": ["reanalysis"],
    "variable": ["divergence"],
    "year": ["1940"],
    "month": ["01"],
    "day": [
        "01", "02", "03",
        "04", "05", "06",
        "07", "08", "09",
        "10", "11", "12",
        "13", "14", "15",
        "16", "17", "18",
        "19", "20", "21",
        "22", "23", "24",
        "25", "26", "27",
        "28", "29", "30",
        "31"
    ],
    "time": ["06:00"],
    "pressure_level": ["1"],
    "data_format": "grib",
    "download_format": "unarchived"
}
client = cdsapi.Client()
client.retrieve(dataset, request).download()

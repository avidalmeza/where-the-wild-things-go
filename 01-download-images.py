#####################################
# 01-download-images.py
# 
# Download HLSS30 images using the {earthaccess} library
#
# Alessandra Vidal Meza
# Last update: Aug 25, 2026
#####################################

import earthaccess

earthaccess.login(persist = True)

short_name = "HLSS30_VI"
version = "2.0"
temporal_range = ("2021-06-01", "2021-08-31")
stl_bbox = (-90.35, 38.55, -90.15, 38.75)
target_indices = ["NDVI", "EVI", "SAVI", "MSAVI"]

granules = earthaccess.search_data(
    short_name = short_name,
    version = version,
    temporal = temporal_range,
    bounding_box = stl_bbox,
)

filtered_links = [] # Initialize

for g in granules:
    for link in g.data_links(): # Iterate over URLs of granules
        if any(f".{i}.tif" in link for i in target_indices):
            filtered_links.append(link)

earthaccess.download(filtered_links, "data/HLS330")

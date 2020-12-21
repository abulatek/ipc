import numpy as np

def finding_hot_pixels(s_read,wd_map=None,lwd_map=None,rwd_map=None,dark_image=None):
    '''
    This function finds hot pixels in a dark frame. One needs to provide a well depth
    map, a dark image, and the read noise for the detector array. If the array is a 
    duplex array, one needs to specify left and right well depth maps.
    '''

    ## 0. Specify lower limit for hot pixels.

    number_of_sigma = 6.
    
    dk_image_mean = np.mean(dark_image)
    dk_image_std = np.std(dark_image)

    limit = dk_image_mean + (number_of_sigma * dk_image_std)
    
    ## 1. Create a list of hot pixels using a filter on the dark current map.
    
    class Pix(object):
        def __init__(cls,x,y,c):
            cls.x = float(x)
            cls.y = float(y)
            cls.c = float(c)
        
    hot_pixels = []

    it = np.nditer(dark_image,flags=['multi_index'])
    while not it.finished:
        pixel = Pix(it.multi_index[1],it.multi_index[0],it[0])
        if pixel.c >= limit:
            hot_pixels.append(pixel)
        nextpixel = it.iternext()

    print "Above {} sigma filter:".format(number_of_sigma),len(hot_pixels)

    ## 2. Note hot pixels which are on the edge of the array. We can't accurately
    # calculate the coupling from these.

    edge_indices = []

    dk_image_size = dark_image.shape

    i = 0
    for hot_pixel in hot_pixels:
        col_coord = int(hot_pixel.x)
        row_coord = int(hot_pixel.y)
        number_of_rows = dk_image_size[0]
        number_of_columns = dk_image_size[1]
        if ((col_coord + 1) == number_of_columns) or \
           ((row_coord + 1) == number_of_rows) or \
           ((col_coord - 1) < 0.) or \
           ((row_coord - 1) < 0.):
            edge_indices.append(i)
        i += 1

    print "Edge hot pixels:",len(edge_indices)

    ## 2.5. If the array is a duplex array, make sure to not count pixels in the
    # center region of the array (nor one column outside the center).

    center_indices = []
    
    if (wd_map is None) and (lwd_map is not None) and (rwd_map is not None):

        i = 0
        for hot_pixel in hot_pixels:
            col_coord = int(hot_pixel.x)
            if (col_coord >= 955) and (col_coord <= 1092):
                center_indices.append(i)
            i += 1

        print "Center hot pixels:",len(center_indices)
    
    ## 3. Note hot pixels from this list that have another hot pixel somewhere in a
    # 5 by 5 square around the event.

    noniso_indices = []

    i = 0
    for hot_pixel in hot_pixels:
        # Hot pixel coordinates:
        col_coord = int(hot_pixel.x)
        row_coord = int(hot_pixel.y)
        # Check if its neighbors are also hot pixels:
        if dark_image[int(row_coord+1), int(col_coord)] >= limit or \
           dark_image[int(row_coord-1), int(col_coord)] >= limit or \
           dark_image[int(row_coord), int(col_coord+1)] >= limit or \
           dark_image[int(row_coord), int(col_coord-1)] >= limit or \
           dark_image[int(row_coord+1), int(col_coord+1)] >= limit or \
           dark_image[int(row_coord+1), int(col_coord-1)] >= limit or \
           dark_image[int(row_coord-1), int(col_coord+1)] >= limit or \
           dark_image[int(row_coord-1), int(col_coord-1)] >= limit or \
           dark_image[int(row_coord+2), int(col_coord)] >= limit or \
           dark_image[int(row_coord-2), int(col_coord)] >= limit or \
           dark_image[int(row_coord), int(col_coord+2)] >= limit or \
           dark_image[int(row_coord), int(col_coord-2)] >= limit or \
           dark_image[int(row_coord+2), int(col_coord+1)] >= limit or \
           dark_image[int(row_coord+2), int(col_coord-1)] >= limit or \
           dark_image[int(row_coord-2), int(col_coord+1)] >= limit or \
           dark_image[int(row_coord-2), int(col_coord-1)] >= limit or \
           dark_image[int(row_coord+1), int(col_coord+2)] >= limit or \
           dark_image[int(row_coord-1), int(col_coord+2)] >= limit or \
           dark_image[int(row_coord+1), int(col_coord-2)] >= limit or \
           dark_image[int(row_coord-1), int(col_coord-2)] >= limit or \
           dark_image[int(row_coord+2), int(col_coord+2)] >= limit or \
           dark_image[int(row_coord+2), int(col_coord-2)] >= limit or \
           dark_image[int(row_coord-2), int(col_coord+2)] >= limit or \
           dark_image[int(row_coord-2), int(col_coord-2)] >= limit:
            noniso_indices.append(i)
        i += 1

    print "Not isolated:",len(noniso_indices)

    ## 4. Ensure that hot pixels aren't saturated.
        
    saturated_indices = []
    
    i = 0
    for hot_pixel in hot_pixels:
        col_coord = int(hot_pixel.x)
        row_coord = int(hot_pixel.y)
        if wd_map is not None:
            well_depth_map = wd_map
        elif (lwd_map is not None) and (col_coord < 955):
            well_depth_map = lwd_map
        else:
            well_depth_map = rwd_map
            # This might catch pixels in the center region of a duplex array.
            # However, they'll get removed anyway with the non-center flag.
        if dark_image[row_coord, col_coord] == well_depth_map[row_coord, col_coord]:
            saturated_indices.append(i)
        i += 1

    print "Saturated:",len(saturated_indices)

    ## 5. Ensure that the four nearest neighbors to pixels are sufficiently
    # symmetric.

    unsymmetric_background_indices = []

    i = 0
    for hot_pixel in hot_pixels:
        col_coord = int(hot_pixel.x)
        row_coord = int(hot_pixel.y)
        neighbors = [dark_image[row_coord+1, col_coord], \
                     dark_image[row_coord-1, col_coord], \
                     dark_image[row_coord, col_coord+1], \
                     dark_image[row_coord, col_coord-1]]
        max_neighbor = float(max(neighbors))
        min_neighbor = float(min(neighbors))
        if (max_neighbor - min_neighbor) > (2. * s_read):
            unsymmetric_background_indices.append(i)
        i += 1

    print "Unsymmetric background:",len(unsymmetric_background_indices)

    '''
    ## *. Remove pixels that are in the bottom half of the detector array.

    bottom_half_indices = []

    i = 0
    for hot_pixel in hot_pixels:
        row_coord = int(hot_pixel.y)
        if (wd_map is None) and (lwd_map is not None) and (rwd_map is not None):
            # The array is 2k by 2k.
            if row_coord >= 1024:
                bottom_half_indices.append(i)
        else:
            # The array is 1k by 1k.
            if row_coord >= 512:
                bottom_half_indices.append(i)
        i += 1

    print "Pixels in the bottom half of the array:",len(bottom_half_indices)
    '''

    ## 6. Remove any flagged pixels from the list.

    flagged_indices = edge_indices + center_indices + noniso_indices + \
                      saturated_indices + unsymmetric_background_indices
                      # + bottom_half_indices
    
    # Remove duplicates from the flagged indices list.
    flagged_indices = list(set(flagged_indices))
    # Sort the flagged indices list in descending order.
    flagged_indices = sorted(flagged_indices,reverse=True)
    for flagged_index in flagged_indices:
        del hot_pixels[flagged_index]
    
    ## 7. Build an array of viable hot pixels for further analysis.

    viable_hps = np.zeros((len(hot_pixels),5))
    for index, hot_pixel in enumerate(hot_pixels):
        viable_hps[index,0] = int(hot_pixel.x)
        viable_hps[index,1] = int(hot_pixel.y)
        viable_hps[index,2] = dark_image[int(hot_pixel.y), int(hot_pixel.x)]

    print "Viable hot pixels:",len(viable_hps)
    print "----------------------------------"
        
    return viable_hps

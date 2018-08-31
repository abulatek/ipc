import numpy as np
from numpy import exp
from astropy.io import fits

def ipc_corr(array,zero_background=True):
    '''
    The goal of this algorithm is to correct an image for the effects of interpixel
    capacitance. It must be passed an array to correct. To use the zero-background
    assumption equation, set zero_background equal to True. To use the equation 
    from Donlon et al. 2018, set zero_background equal to False.
    '''
    
    init_shape = np.shape(array)
    original = array[4:init_shape[0]-4,4:init_shape[1]-4]
    working = np.copy(original)
    final = np.copy(original)

    shape = np.shape(original)

    ## 1. Do coupling for neighbor down.

    # Shift working copy up by one pixel.

    working = np.roll(working, -1, axis = 0)
    wc = working[0:shape[0]-1,:]
    oc = original[0:shape[0]-1,:]

    # Calculate coupling between pixels at the same index in oc and wc.

    if not zero_background:
        # The expressions below are from Donlon et al. 2018, and assume some
        # dependence of local coupling on neighbor strength.
        coupling_in = (0.4 * exp(-np.absolute(wc - oc)/20000.) + 0.4 * exp(-np.sqrt(oc**2. + wc**2.)/(10000. * np.sqrt(2.))) + 0.65) / 100.
        coupling_out = (0.4 * exp(-np.absolute(oc - wc)/20000.) + 0.4 * exp(-np.sqrt(oc**2. + wc**2.)/(10000. * np.sqrt(2.))) + 0.65) / 100.

    elif zero_background:
        # The expressions below assume no background illumination.
        coupling_in = (0.0992 * exp(-wc / 2202.9) + 0.979) / 100.
        coupling_out = (0.0992 * exp(-oc / 2202.9) + 0.979) / 100.
    
    # Subtract (coupling_in * neighbor down) from the central pixel in final.

    final[0:shape[0]-1,:] -= coupling_in * final[1:shape[0],:]
    
    # Add (coupling_out * central) to the central pixel in final.

    final[0:shape[0]-1,:] += coupling_out * final[0:shape[0]-1,:]

    # Shift working copy back down by one pixel.

    working = np.roll(working, 1, axis = 0)

    ## 2. Do coupling for neighbor up.

    # Shift working copy down by one pixel.

    working = np.roll(working, 1, axis = 0)
    wc = working[1:shape[0],:]
    oc = original[1:shape[0],:]

    # Calculate coupling between pixels at the same index in oc and wc.

    if not zero_background:
        # The expressions below are from Donlon et al. 2018, and assume some
        # dependence of local coupling on neighbor strength.
        coupling_in = (0.4 * exp(-np.absolute(wc - oc)/20000.) + 0.4 * exp(-np.sqrt(oc**2. + wc**2.)/(10000. * np.sqrt(2.))) + 0.65) / 100.
        coupling_out = (0.4 * exp(-np.absolute(oc - wc)/20000.) + 0.4 * exp(-np.sqrt(oc**2. + wc**2.)/(10000. * np.sqrt(2.))) + 0.65) / 100.

    elif zero_background:
        # The expressions below assume no background illumination.
        coupling_in = (0.0992 * exp(-wc / 2202.9) + 0.979) / 100.
        coupling_out = (0.0992 * exp(-oc / 2202.9) + 0.979) / 100.

    # Subtract (coupling_in * neighbor up) from the central pixel in final.

    final[1:shape[0],:] -= coupling_in * final[0:shape[0]-1,:]
    
    # Add (coupling_out * central) to the central pixel in final.

    final[1:shape[0],:] += coupling_out * final[1:shape[0],:]

    # Shift working copy back up by one pixel.

    working = np.roll(working, -1, axis = 0)

    ## 3. Do coupling for neighbor right.

    # Shift working copy to the left by one pixel.

    working = np.roll(working, -1, axis = 1)
    wc = working[:,0:shape[1]-1]
    oc = original[:,0:shape[1]-1]

    # Calculate coupling between pixels at the same index in oc and wc.

    if not zero_background:
        # The expressions below are from Donlon et al. 2018, and assume some
        # dependence of local coupling on neighbor strength.
        coupling_in = (0.4 * exp(-np.absolute(wc - oc)/20000.) + 0.4 * exp(-np.sqrt(oc**2. + wc**2.)/(10000. * np.sqrt(2.))) + 0.65) / 100.
        coupling_out = (0.4 * exp(-np.absolute(oc - wc)/20000.) + 0.4 * exp(-np.sqrt(oc**2. + wc**2.)/(10000. * np.sqrt(2.))) + 0.65) / 100.

    elif zero_background:
        # The expressions below assume no background illumination.
        coupling_in = (0.0992 * exp(-wc / 2202.9) + 0.979) / 100.
        coupling_out = (0.0992 * exp(-oc / 2202.9) + 0.979) / 100.

    # Subtract (coupling_in * neighbor right) from the central pixel in final.

    final[:,0:shape[1]-1] -= coupling_in * final[:,1:shape[1]]
    
    # Add (coupling_out * central) to the central pixel in final.

    final[:,0:shape[1]-1] += coupling_out * final[:,0:shape[1]-1]

    # Shift working copy back to the right by one pixel.

    working = np.roll(working, 1, axis = 1)

    ## 4. Do coupling for neighbor left.

    # Shift working copy to the right by one pixel.

    working = np.roll(working, 1, axis = 1)
    wc = working[:,1:shape[1]]
    oc = working[:,1:shape[1]]

    # Calculate coupling between pixels at the same index in oc and wc.

    if not zero_background:
        # The expressions below are from Donlon et al. 2018, and assume some
        # dependence of local coupling on neighbor strength.
        coupling_in = (0.4 * exp(-np.absolute(wc - oc)/20000.) + 0.4 * exp(-np.sqrt(oc**2. + wc**2.)/(10000. * np.sqrt(2.))) + 0.65) / 100.
        coupling_out = (0.4 * exp(-np.absolute(oc - wc)/20000.) + 0.4 * exp(-np.sqrt(oc**2. + wc**2.)/(10000. * np.sqrt(2.))) + 0.65) / 100.

    elif zero_background:
        # The expressions below assume no background illumination.
        coupling_in = (0.0992 * exp(-wc / 2202.9) + 0.979) / 100.
        coupling_out = (0.0992 * exp(-oc / 2202.9) + 0.979) / 100.

    # Subtract (coupling_in * neighbor left) from the central pixel in final.

    final[:,1:shape[1]] -= coupling_in * final[:,0:shape[1]-1]
    
    # Add (coupling_out * central) to the central pixel in final.

    final[:,1:shape[1]] += coupling_out * final[:,1:shape[1]]

    # Shift working copy back to the left by one pixel.

    working = np.roll(working, -1, axis = 1)

    ## 5. Add the reference pixels back to final and return the final array.

    final_array = np.copy(array)
    final_array[4:init_shape[0]-4,4:init_shape[1]-4] = final

    return final_array

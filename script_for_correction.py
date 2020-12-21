from astropy.io import fits
import numpy as np
from correct_for_ipc import ipc_corr

def main():
    array_name = "H1RG-17354"

    #cube_filename = "/media/data/"+array_name+"/nasaames_radtest/secondrun/2014_oct_02_davis_beam/35K_6/Picture_1/Frame_1.fits"
    cube_filename = "LEDA44055.HST.blue.fits"
    cube = fits.getdata(cube_filename,dtype=float)

    corrected_images = []

    for fr in cube:
        corrected_fr1 = ipc_corr(fr,zero_background=True)
        corrected_fr2 = ipc_corr(corrected_fr1,zero_background=True)
        corrected_fr3 = ipc_corr(corrected_fr2,zero_background=True)
        corrected_fr4 = ipc_corr(corrected_fr3,zero_background=True)
        corrected_images.append(corrected_fr4)

    corrected_cube = np.stack(corrected_images)

    fits.writeto('H1RG-17354_corrected.fits',corrected_cube)

    '''
    array_name = "H1RG-17346"

    cube_filename = "/media/data/"+array_name+"/nasaames_radtest/2014_Sep_04_Rad/Beam-12MeV/35K_10/Picture_1/Frame_1.fits"
    cube = fits.getdata(cube_filename,dtype=float)

    corrected_images = []

    for fr in cube:
        corrected_fr1 = ipc_corr(fr,zero_background=True)
        corrected_fr2 = ipc_corr(corrected_fr1,zero_background=True)
        corrected_fr3 = ipc_corr(corrected_fr2,zero_background=True)
        corrected_fr4 = ipc_corr(corrected_fr3,zero_background=True)
        corrected_images.append(corrected_fr4)

    corrected_cube = np.stack(corrected_images)

    fits.writeto('H1RG-17346_corrected.fits',corrected_cube)
    '''

if __name__ == "__main__":
    main()

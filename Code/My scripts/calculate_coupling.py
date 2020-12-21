from astropy.io import fits
from find_hot_pixels import finding_hot_pixels
import numpy as np
from numpy import exp
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit
import time


### This file was used to characterize the alpha versus signal strength relationship
### assuming zero background.


start_time = time.time()


def exp_decay(x, A, B, a_inf):
    # Returns a function of exponential decay as a function of x given an amplitude (A),
    # a decay rate (B), and a y value at infinity (a_inf).

    return A * exp(-x / B) + a_inf


## 1. Administrative tasks.

temp = "35"
bias = "150"
number_of_ramps = 12

position_str = "3"
position = position_str.zfill(3)

position2_str = "2"
position2 = position2_str.zfill(3)

width = 12600.
binned = True
adjust_once = True
adjust_twice = True

# Calculate the read noise.

frame_next_file = "/home/abulatek/Nix2/H2RG-18481/readnoise{0}K_{1}mV/readnoise{0}K_{1}mV_040_030.fits".format(temp,bias)
frame_last_file = "/home/abulatek/Nix2/H2RG-18481/readnoise{0}K_{1}mV/readnoise{0}K_{1}mV_040_029.fits".format(temp,bias)
frame_next = fits.getdata(frame_next_file,dtype=float)
frame_last = fits.getdata(frame_last_file,dtype=float)
diff = frame_next - frame_last
sigma_CDS = np.std(diff)
read_noise = sigma_CDS / np.sqrt(2)

# Define well depth map.

left_well_depth_map_file = \
"/home/meghan/det/data/H2RG_data/H2RG-18481/dknwell/Leftside/{0}K_{1}mV_well.fits".format(temp,bias)
left_well_depth_map = fits.getdata(left_well_depth_map_file,dtype=float)
right_well_depth_map_file = \
"/home/meghan/det/data/H2RG_data/H2RG-18481/dknwell/Rightside/{0}K_{1}mV_well.fits".format(temp,bias)
right_well_depth_map = fits.getdata(right_well_depth_map_file,dtype=float)

# Collect images from dark ramps.

dark_images = []

for ramp in range(1,number_of_ramps+1):
    ramp_string = str(ramp).zfill(3)
    up_ramp_file = "/home/abulatek/Nix2/H2RG-18481/readnoise{0}K_{1}mV/readnoise{0}K_{1}mV_{2}_{3}.fits".format(temp,bias,ramp_string,position)
    up_ramp = fits.getdata(up_ramp_file,dtype=float)
    pedestal_file = "/home/abulatek/Nix2/H2RG-18481/readnoise{0}K_{1}mV/readnoise{0}K_{1}mV_{2}_001.fits".format(temp,bias,ramp_string)
    pedestal = fits.getdata(pedestal_file,dtype=float)
    CDS_image = up_ramp - pedestal
    dark_images.append(CDS_image)

for ramp in range(1,number_of_ramps+1):
    ramp_string = str(ramp).zfill(3)
    up_ramp_file = "/home/abulatek/Nix2/H2RG-18481/readnoise{0}K_{1}mV/readnoise{0}K_{1}mV_{2}_{3}.fits".format(temp,bias,ramp_string,position2)
    up_ramp = fits.getdata(up_ramp_file,dtype=float)
    pedestal_file = "/home/abulatek/Nix2/H2RG-18481/readnoise{0}K_{1}mV/readnoise{0}K_{1}mV_{2}_001.fits".format(temp,bias,ramp_string)
    pedestal = fits.getdata(pedestal_file,dtype=float)
    CDS_image = up_ramp - pedestal
    dark_images.append(CDS_image)

## 2. Find viable hot pixels for calculating the coupling coefficient. The output
# of this is an array with five columns and n rows where n is the number of viable
# hot pixels found. The columns are as follows.
# Column [0]: x-coordinate (col) of the hot pixel.
# Column [1]: y-coordinate (row) of the hot pixel.
# Column [2]: value of the hot pixel in the dark frame.
# Column [3]: coupling coefficient for the hot pixel in percent (to be entered).
# Column [4]: signal strength for the hot pixel (to be entered).

arrays = []

for dk_im in dark_images:
    viable_hps = finding_hot_pixels(read_noise,lwd_map=left_well_depth_map,rwd_map=right_well_depth_map,dark_image=dk_im)

    ## 3. Calculate the coupling coefficient alpha for each of the hot pixels, and also
    # record the signal strength.
    
    for index, hot_pixel in enumerate(viable_hps):
        col_coord = int(hot_pixel[0])
        row_coord = int(hot_pixel[1])
        value = hot_pixel[2]
        four_nearest_neighbors = [dk_im[int(row_coord+1), int(col_coord)], \
                                  dk_im[int(row_coord-1), int(col_coord)], \
                                  dk_im[int(row_coord), int(col_coord+1)], \
                                  dk_im[int(row_coord), int(col_coord-1)]]
        five_by_five = [dk_im[int(row_coord+2), int(col_coord)], \
                        dk_im[int(row_coord-2), int(col_coord)], \
                        dk_im[int(row_coord), int(col_coord+2)], \
                        dk_im[int(row_coord), int(col_coord-2)], \
                        dk_im[int(row_coord+2), int(col_coord+1)], \
                        dk_im[int(row_coord+2), int(col_coord-1)], \
                        dk_im[int(row_coord-2), int(col_coord+1)], \
                        dk_im[int(row_coord-2), int(col_coord-1)], \
                        dk_im[int(row_coord+2), int(col_coord+2)], \
                        dk_im[int(row_coord+2), int(col_coord-2)], \
                        dk_im[int(row_coord-2), int(col_coord+2)], \
                        dk_im[int(row_coord-2), int(col_coord-2)], \
                        dk_im[int(row_coord+1), int(col_coord+2)], \
                        dk_im[int(row_coord-1), int(col_coord+2)], \
                        dk_im[int(row_coord+1), int(col_coord-2)], \
                        dk_im[int(row_coord-1), int(col_coord-2)]]
        avg_neighbor = np.mean(four_nearest_neighbors)
        local_median = np.median(five_by_five)
        # Alpha is in decimal form here, NOT percent form.
        alpha = ((avg_neighbor - local_median)/    \
                ((4. * (avg_neighbor - local_median)) + (value - local_median)))
        signal_strength = (value - local_median)
        # Converting alpha to percent form below.
        alpha_percent = alpha * 100.
        viable_hps[index,3] = alpha_percent
        viable_hps[index,4] = signal_strength

    ## 4. Create an array with just the signal strength and coupling coefficient for
    # each hot pixel.

    individual_array = np.zeros((len(viable_hps[:,3]),2))

    for index, pixel in enumerate(individual_array):
        pixel[0] = viable_hps[index,3]
        pixel[1] = viable_hps[index,4]

    if len(dark_images) > 1:
        arrays.append(individual_array)
    else:
        coupling_vs_signal_strength = individual_array

if len(arrays) > 1:
    coupling_vs_signal_strength = np.vstack(arrays)

## 5. Average the values with a fixed number of points in each bin and get error bars.

alpha_std_devs = []

cvs_sorted = coupling_vs_signal_strength[coupling_vs_signal_strength[:,1].argsort()]
zstar = 1.645

alpha_vals = cvs_sorted[:,0]
binned_alpha_vals = alpha_vals[:int((alpha_vals.size // width) * width)].reshape(-1, int(width))
avg_alpha_vals = binned_alpha_vals.mean(axis=1)

alpha_vals_err = []

for b in binned_alpha_vals:
    b_stddev = b.std()
    alpha_std_devs.append(b_stddev)
    CI = (zstar * b_stddev) / np.sqrt(width)
    alpha_vals_err.append(CI)

signal_strength_vals = cvs_sorted[:,1]
binned_signal_strength_vals = signal_strength_vals[:int((signal_strength_vals.size // width) * width)].reshape(-1, int(width))
avg_signal_strength_vals = binned_signal_strength_vals.mean(axis=1)

signal_strength_vals_err = []

for c in binned_signal_strength_vals:
    c_stddev = c.std()
    signal_strength_vals_err.append(c_stddev)

if adjust_once:
    ## 5.1. Implement an adjustable binsize based on calculated errors.

    # Set the limit for the error bar when we'll switch to the smaller number of points
    # per bin.
    err_limit = 0.5*max(signal_strength_vals_err)

    # Loop through the bins and note when the uncertainty hits the limit.
    for ind, val in enumerate(signal_strength_vals_err):
        if val >= err_limit:
            split_ind = ind
            break

    # Set the new number of points per bin.
    split_width = 0.5 * width

    # Note the points and error bars before the split.
    avg_alpha_vals_before = avg_alpha_vals[:split_ind]
    alpha_vals_err_before = alpha_vals_err[:split_ind]
    avg_signal_strength_vals_before = avg_signal_strength_vals[:split_ind]
    signal_strength_vals_err_before = signal_strength_vals_err[:split_ind]

    # Rebin the alpha values after the split.
    alpha_vals_after = np.concatenate(binned_alpha_vals[split_ind:])
    binned_alpha_vals_after = alpha_vals_after[:int((alpha_vals_after.size // split_width) * split_width)].reshape(-1, int(split_width))
    avg_alpha_vals_after = binned_alpha_vals_after.mean(axis=1)

    # Recalculate the error bars for alpha.
    alpha_vals_err_after = []

    for d in binned_alpha_vals_after:
        d_stddev = d.std()
        CI = (zstar * d_stddev) / np.sqrt(split_width)
        alpha_vals_err_after.append(CI)

    # Rebin the signal strength values after the split.
    signal_strength_vals_after = np.concatenate(binned_signal_strength_vals[split_ind:])
    binned_signal_strength_vals_after = signal_strength_vals_after[:int((signal_strength_vals_after.size // split_width) * split_width)].reshape(-1, int(split_width))
    avg_signal_strength_vals_after = binned_signal_strength_vals_after.mean(axis=1)

    # Recalculate the error bars for signal strength.
    signal_strength_vals_err_after = []
    
    for f in binned_signal_strength_vals_after:
        f_stddev = f.std()
        signal_strength_vals_err_after.append(f_stddev)

    # Recombine the final arrays of alpha, signal strength, and their errors.
    avg_alpha_vals = np.concatenate((avg_alpha_vals_before,avg_alpha_vals_after))
    avg_signal_strength_vals = np.concatenate((avg_signal_strength_vals_before,avg_signal_strength_vals_after))
    alpha_vals_err = np.concatenate((alpha_vals_err_before,alpha_vals_err_after))
    signal_strength_vals_err = np.concatenate((signal_strength_vals_err_before,signal_strength_vals_err_after))

if adjust_twice:
    ## 5.2. Adjust the binsize again to reduce error in signal strength direction.
    # This will need to be customized for a given dataset.

    # From observing the data, note the index which will begin the split, and define
    # the new width.
    split_ind2 = 12
    split_width2 = 0.5 * split_width

    # Note the points and error bars before the second split.
    avg_alpha_vals_before2 = avg_alpha_vals[:split_ind2]
    alpha_vals_err_before2 = alpha_vals_err[:split_ind2]
    avg_signal_strength_vals_before2 = avg_signal_strength_vals[:split_ind2]
    signal_strength_vals_err_before2 = signal_strength_vals_err[:split_ind2]

    # Rebin the alpha values after the split.
    # I am using the -1 index instead of calculating what the new index would be.
    alpha_vals_after2 = binned_alpha_vals_after[-1]
    binned_alpha_vals_after2 = alpha_vals_after2[:int((alpha_vals_after2.size // split_width2) * split_width2)].reshape(-1, int(split_width2))
    avg_alpha_vals_after2 = binned_alpha_vals_after2.mean(axis=1)

    # Recalculate the error bars for alpha.
    alpha_vals_err_after2 = []

    for g in binned_alpha_vals_after2:
        g_stddev = g.std()
        CI = (zstar * g_stddev) / np.sqrt(split_width)
        alpha_vals_err_after2.append(CI)

    # Rebin the signal strength values after the split.
    signal_strength_vals_after2 = binned_signal_strength_vals_after[-1]
    binned_signal_strength_vals_after2 = signal_strength_vals_after2[:int((signal_strength_vals_after2.size // split_width2) * split_width2)].reshape(-1, int(split_width2))
    avg_signal_strength_vals_after2 = binned_signal_strength_vals_after2.mean(axis=1)

    # Recalculate the error bars for signal strength.
    signal_strength_vals_err_after2 = []
    
    for h in binned_signal_strength_vals_after2:
        h_stddev = h.std()
        signal_strength_vals_err_after2.append(h_stddev)

    # Recombine the final arrays of alpha, signal strength, and their errors.
    avg_alpha_vals = np.concatenate((avg_alpha_vals_before2,avg_alpha_vals_after2))
    avg_signal_strength_vals = np.concatenate((avg_signal_strength_vals_before2,avg_signal_strength_vals_after2))
    alpha_vals_err = np.concatenate((alpha_vals_err_before2,alpha_vals_err_after2))
    signal_strength_vals_err = np.concatenate((signal_strength_vals_err_before2,signal_strength_vals_err_after2))
    
## 7. Fit an exponential function to the binned data.

bounds = ((0,-np.inf,-np.inf),(np.inf,np.inf,np.inf))
guess = [5,364,0.82]
popt, pcov = curve_fit(exp_decay,avg_signal_strength_vals,avg_alpha_vals,p0=guess,bounds=bounds)

perr3 = 3*np.sqrt(np.diag(pcov))

A = popt[0]
A_err = perr3[0]
B = popt[1]
B_err = perr3[1]
a_inf = popt[2]
a_inf_err = perr3[2]

print "This fit has the following parameters."
print "A: {:.3f} +/- {:.5f}".format(A,A_err)
print "B: {:.3f} +/- {:.5f}".format(B,B_err)
print "alpha at infinity: {:.3f} +/- {:.5f}".format(a_inf,a_inf_err)
if adjust_once:
    print "{} points are from the first bin size.".format(split_ind)
if adjust_once and adjust_twice:
    print "{} points are from the second bin size.".format(split_ind2-split_ind)

## 8. Plot the data and the fit.

#x = np.linspace(0,max(avg_signal_strength)+1000.,10000)
x = np.linspace(0,40000,10000)
alpha_fit = exp_decay(x, A, B, a_inf)

textstr = 'A={:.3f} +/- {:.5f}\nB={:.3f} +/- {:.5f}\nalpha_inf={:.3f} +/- {:.5f}'.format(A,A_err,B,B_err,a_inf,a_inf_err)
props = dict(boxstyle='round',facecolor='#e5e5e5')

end_time = time.time()
total_time = end_time - start_time
print ".................................."
print "It took {:.2f} seconds.".format(total_time)

if not binned:
    plt.scatter(signal_strength_vals,alpha_vals)
    #plt.title("{}K, 150mV bias, sample {}, {} viable hot pixels".format(temp, position_str, len(signal_strength_vals)), fontsize=12)
    plt.title("Unbinned coupling versus signal strength for {} hot pixels".format(len(signal_strength_vals)), fontsize=16)
    plt.xticks(fontsize=16)
    plt.yticks(fontsize=16)
    plt.xlabel("Signal strength (ADU)", fontsize=16)
    plt.ylabel("Coupling coefficient (%)", fontsize=16)
    #plt.savefig("{}K_150mV_sample{}_outof{}.png".format(temp, position_str, len(viable_hps)))
    plt.show()
elif binned:
    plt.plot(x, alpha_fit)
    plt.errorbar(avg_signal_strength_vals, avg_alpha_vals, xerr=signal_strength_vals_err, yerr=alpha_vals_err, fmt='o')
    plt.text(max(avg_signal_strength_vals), 1.05, textstr, verticalalignment='top', horizontalalignment='left', bbox=props)
    if adjust_once and not adjust_twice:
        plt.title("{}K, 150mV bias, sample {}, {}/{} points per bin, {} viable hot pixels".format(temp, position_str, int(width), int(split_width), len(signal_strength_vals)), fontsize=12)
    elif adjust_once and adjust_twice:
        plt.title("{}K, 150mV bias, sample {}, {}/{}/{} points per bin, {} viable hot pixels".format(temp, position_str, int(width), int(split_width), int(split_width2), len(signal_strength_vals)), fontsize=12)
    elif not adjust_once and not adjust_twice:
        plt.title("{}K, 150mV bias, sample {}, {} points per bin, {} viable hot pixels".format(temp, position_str, int(width), len(signal_strength_vals)), fontsize=12)
    plt.xlabel("Signal strength (ADU)")
    plt.ylabel("Coupling coefficient (%)")
    #plt.savefig("{}K_150mV_sample{}_width{}_outof{}.png".format(temp, position_str, width, len(viable_hps)))
    plt.show()

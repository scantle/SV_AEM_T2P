import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import gstools as gs
from pathlib import Path

# -------------------------------------------------------------------------------------------------------------------- #
# Settings
# -------------------------------------------------------------------------------------------------------------------- #

# Directories
data_dir = Path('02_Models/Texture2Par_onlytexture_3D/')

# Texture files
textures = ['FINE', 'MIXED_FINE', 'SAND', 'MIXED_COARSE', 'VERY_COARSE']


# -------------------------------------------------------------------------------------------------------------------- #
# Functions
# -------------------------------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------------------------------- #
# Main
# -------------------------------------------------------------------------------------------------------------------- #

variogram_data = {}

# -------------------------------------------------------------------------------------------------------------------- #
# Fine
# -------------------------------------------------------------------------------------------------------------------- #
tex = textures[0]
fine_df = pd.read_csv(data_dir / f"t2p_{tex}_logxyz.csv", na_values=-999)
fine_df = fine_df.dropna()

x = fine_df.X.to_numpy()
y = fine_df.Y.to_numpy()
z = fine_df.Z.to_numpy()
values = fine_df.Layer1.to_numpy()

bin_center, gamma = gs.vario_estimate((x,y,z), values, sampling_size=1000)

model = gs.Spherical(dim=3, var=1.0, nugget=0.0)
fit = model.fit_variogram(bin_center, gamma, nugget=True, return_r2=True)

ax = model.plot(x_max=max(bin_center))
ax.scatter(bin_center, gamma)
ax.set_xlabel(r"Distance $r$ / m")
ax.set_ylabel(r"Variogram")

fig = ax.get_figure()
fig.tight_layout()

print("Coefficient of determination of the fit R² = {:.3}".format(fit[2]))
print("semivariogram model (isotropic):")
print(model)

# Compute directional variograms along X, Y, and Z axes
bin_x, gamma_x = gs.vario_estimate((x, y, z), values, direction=[1, 0, 0], sampling_size=5000)
bin_y, gamma_y = gs.vario_estimate((x, y, z), values, direction=[0, 1, 0], sampling_size=5000)

# Plot the xy directional variograms
plt.figure()
plt.scatter(bin_x, gamma_x, label='X-Direction', alpha=0.7)
plt.scatter(bin_y, gamma_y, label='Y-Direction', alpha=0.7)
plt.xlabel(r"Distance $r$ / m")
plt.ylabel(r"Variogram")
plt.legend()
plt.title("Directional Variograms")
plt.tight_layout()
plt.show()

# Finer binning strategy for the Z direction
bin_z_fine, gamma_z_fine = gs.vario_estimate((x, y, z), values,
                                             direction=[0, 0, 1],
                                             sampling_size=10000,
                                             bin_edges=np.linspace(0, 800, 35))

# Plot the Z-direction variogram with finer bins
plt.figure()
plt.scatter(bin_z_fine, gamma_z_fine, label='Z-Direction (Fine Binning)', color='purple')
plt.xlabel(r"Distance $r$ / m")
plt.ylabel(r"Variogram")
plt.title("Z-Direction Variogram with Fine Binning")
plt.axhline(0, color='gray', linewidth=0.5)
plt.axvline(0, color='gray', linewidth=0.5)
plt.legend()
plt.tight_layout()
plt.show()

# -------------------------------------------------------------------------------------------------------------------- #
# What's our principal direction?

# # Define the range of angles to test
# angles = np.arange(25, 50, 2.5)  # You can adjust the step size as needed
# ranges = []
#
# # Loop through each angle and calculate the range of the variogram
# for angle in angles:
#     # Direction as a unit vector in the horizontal plane
#     direction = [np.cos(np.radians(angle)), np.sin(np.radians(angle)), 0]
#
#     # Compute the directional variogram
#     bin_center, gamma = gs.vario_estimate((x, y, z), values, direction=direction, sampling_size=5000)
#
#     # Fit an isotropic model to the directional variogram
#     model = gs.Exponential(dim=3, var=1.0, nugget=0.0)
#     fit = model.fit_variogram(bin_center, gamma, nugget=True)
#
#     # Store the range and the corresponding angle
#     ranges.append((angle, model.len_scale))
#     print(f"Angle: {angle:.1f}°, Range: {model.len_scale:.2f} m")
#
# # Find the angle with the longest range
# best_angle, best_range = max(ranges, key=lambda x: x[1])
# print(f"\nBest angle: {best_angle:.1f}° with range: {best_range:.2f} m")
#
# # Plot the ranges vs. angles
# plt.figure()
# plt.plot([r[0] for r in ranges], [r[1] for r in ranges], marker='o')
# plt.xlabel("Direction (Degrees)")
# plt.ylabel("Range (m)")
# plt.title("Range vs. Direction")
# plt.grid(True)
# plt.tight_layout()
# plt.show()

# -------------------------------------------------------------------------------------------------------------------- #
# Towards final variograms

# 1. Estimate the range in the principal (maximum) direction
#principal_angle = 37.5
vgm_typ = gs.Exponential
#xy_direction_max = [np.cos(np.radians(principal_angle)), np.sin(np.radians(principal_angle)), 0]
bin_center_max, gamma_max = gs.vario_estimate((x, y, z), values,
                                              direction=[1,1,0], sampling_size=10000)

model_max = vgm_typ(dim=3, var=1.0, nugget=0.0)
fit_max = model_max.fit_variogram(bin_center_max, gamma_max, nugget=True, return_r2=True)
range_max = model_max.len_scale
print(f"Range in principal (maximum) direction: {range_max:.2f} m, r^2: {fit_max[2]}")

# # 2. Estimate the range in the perpendicular (minimum) direction
# min_angle = (principal_angle + 90) % 180
# xy_direction_min = [np.cos(np.radians(min_angle)), np.sin(np.radians(min_angle)), 0]
# bin_center_min, gamma_min = gs.vario_estimate((x, y, z), values,
#                                               direction=xy_direction_min, sampling_size=10000)
#
# model_min = vgm_typ(dim=3, var=1.0, nugget=0.0)
# fit_min = model_min.fit_variogram(bin_center_min, gamma_min, nugget=True, return_r2=True)
# range_min = model_min.len_scale
# print(f"Range in perpendicular (minimum) direction: {range_min:.2f} m, r^2: {fit_min[2]}")

# 3. Estimate the range in the vertical (Z) direction
bin_center_z, gamma_z = gs.vario_estimate((x, y, z), values,
                                          direction=[0, 0, 1],
                                          sampling_size=10000,
                                          bin_edges=np.linspace(0, 600, 50))

model_z = vgm_typ(dim=3, var=1.0, nugget=0.0)
fit_z = model_z.fit_variogram(bin_center_z, gamma_z, nugget=True, return_r2=True)

plt.figure()
plt.scatter(bin_center_z, gamma_z, color='purple')
model_z.plot(ax=plt.gca(), x_max=max(bin_center_z))
plt.xlabel(r"Distance $r$ / m")
plt.ylabel(r"Variogram")
plt.title("Z-Direction Variogram Model")
plt.legend()

range_z = model_z.len_scale
print(f"Range in vertical (Z) direction: {range_z:.2f} m, r^2: {fit_z[2]}")

# 4. Calculate anisotropy ratios
anis_xy = 1  #range_min / range_max
anis_z = range_z / range_max
print(f"XY Anisotropy: {anis_xy:.3f}")
print(f"Vertical Anisotropy (e_z): {anis_z:.3f}")

# 5. Fit the final anisotropic variogram
model_aniso = vgm_typ(dim=3, var=1.0, nugget=0.0, anis=[1, anis_xy, anis_z], angles=[0, 0, 0])
fit_aniso = model_aniso.fit_variogram(bin_center_max, gamma_max, nugget=True, return_r2=True)

# Plot the final anisotropic variogram
plt.figure()
plt.scatter(bin_center_max, gamma_max, label=f'Isotropic XY Fit)', color='green')
model_aniso.plot(ax=plt.gca(), x_max=max(bin_center_max))
plt.xlabel(r"Distance $r$ / m")
plt.ylabel(r"Variogram")
plt.title("Anisotropic Variogram Model")
plt.legend()

# Add text box with parameters
params = (f"Nugget: {model_aniso.nugget:.3f}\n"
          f"Sill: {model_aniso.var:.3f}\n"
          f"Range Max: {range_max:.2f} m\n"
          f"Range Vert: {range_z:.2f} m\n"
          f"XY Anisotropy: {anis_xy:.3f}\n"
          f"Vertical Anisotropy (e_z): {anis_z:.3f}\n"
          f"R²: {fit_aniso[2]:.3f}")
plt.gca().text(0.95, 0.05, params, transform=plt.gca().transAxes,
               fontsize=10, verticalalignment='bottom', horizontalalignment='right',
               bbox=dict(facecolor='white', alpha=0.8, edgecolor='gray'))

plt.tight_layout()
plt.show()
# -------------------------------------------------------------------------------------------------------------------- #
# Mixed Fine
# -------------------------------------------------------------------------------------------------------------------- #

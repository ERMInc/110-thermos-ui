(ns thermos-frontend.chart.colors)

(def ^:const brewer-schemes
  {:spectral {3 ["#fb8c58", "#fefebe", "#98d493"], 4 ["#d6181b", "#fcad60", "#aadca3", "#2a82b9"], 5 ["#d6181b", "#fcad60", "#fefebe", "#aadca3", "#2a82b9"], 6 ["#d43d4e", "#fb8c58", "#fddf8a", "#e5f497", "#98d493", "#3187bc"], 7 ["#d43d4e", "#fb8c58", "#fddf8a", "#fefebe", "#e5f497", "#98d493", "#3187bc"], 8 ["#d43d4e", "#f36c42", "#fcad60", "#fddf8a", "#e5f497", "#aadca3", "#65c1a4", "#3187bc"], 9 ["#d43d4e", "#f36c42", "#fcad60", "#fddf8a", "#fefebe", "#e5f497", "#aadca3", "#65c1a4", "#3187bc"], 10 ["#9d0041", "#d43d4e", "#f36c42", "#fcad60", "#fddf8a", "#e5f497", "#aadca3", "#65c1a4", "#3187bc", "#5d4ea1"], 11 ["#9d0041", "#d43d4e", "#f36c42", "#fcad60", "#fddf8a", "#fefebe", "#e5f497", "#aadca3", "#65c1a4", "#3187bc", "#5d4ea1"], :type :div}

   :rdylgn {3 ["#fb8c58", "#fefebe", "#90ce5f"], 4 ["#d6181b", "#fcad60", "#a5d869", "#199540"], 5 ["#d6181b", "#fcad60", "#fefebe", "#a5d869", "#199540"], 6 ["#d62f26", "#fb8c58", "#fddf8a", "#d8ee8a", "#90ce5f", "#19974f"], 7 ["#d62f26", "#fb8c58", "#fddf8a", "#fefebe", "#d8ee8a", "#90ce5f", "#19974f"], 8 ["#d62f26", "#f36c42", "#fcad60", "#fddf8a", "#d8ee8a", "#a5d869", "#65bc62", "#19974f"], 9 ["#d62f26", "#f36c42", "#fcad60", "#fddf8a", "#fefebe", "#d8ee8a", "#a5d869", "#65bc62", "#19974f"], 10 ["#a40025", "#d62f26", "#f36c42", "#fcad60", "#fddf8a", "#d8ee8a", "#a5d869", "#65bc62", "#19974f", "#006736"], 11 ["#a40025", "#d62f26", "#f36c42", "#fcad60", "#fddf8a", "#fefebe", "#d8ee8a", "#a5d869", "#65bc62", "#19974f", "#006736"], :type :div}

   :rdbu {3 ["#ee8961", "#f6f6f6", "#66a8ce"], 4 ["#c9001f", "#f3a481", "#91c4dd", "#0470af"], 5 ["#c9001f", "#f3a481", "#f6f6f6", "#91c4dd", "#0470af"], 6 ["#b1172a", "#ee8961", "#fcdac6", "#d0e4ef", "#66a8ce", "#2065ab"], 7 ["#b1172a", "#ee8961", "#fcdac6", "#f6f6f6", "#d0e4ef", "#66a8ce", "#2065ab"], 8 ["#b1172a", "#d55f4c", "#f3a481", "#fcdac6", "#d0e4ef", "#91c4dd", "#4292c2", "#2065ab"], 9 ["#b1172a", "#d55f4c", "#f3a481", "#fcdac6", "#f6f6f6", "#d0e4ef", "#91c4dd", "#4292c2", "#2065ab"], 10 ["#66001e", "#b1172a", "#d55f4c", "#f3a481", "#fcdac6", "#d0e4ef", "#91c4dd", "#4292c2", "#2065ab", "#042f60"], 11 ["#66001e", "#b1172a", "#d55f4c", "#f3a481", "#fcdac6", "#f6f6f6", "#d0e4ef", "#91c4dd", "#4292c2", "#2065ab", "#042f60"], :type :div}

   :piyg {3 ["#e8a2c8", "#f6f6f6", "#a0d669"], 4 ["#cf1b8a", "#f0b5d9", "#b7e085", "#4cab25"], 5 ["#cf1b8a", "#f0b5d9", "#f6f6f6", "#b7e085", "#4cab25"], 6 ["#c41a7c", "#e8a2c8", "#fcdfee", "#e5f4cf", "#a0d669", "#4c9120"], 7 ["#c41a7c", "#e8a2c8", "#fcdfee", "#f6f6f6", "#e5f4cf", "#a0d669", "#4c9120"], 8 ["#c41a7c", "#dd76ad", "#f0b5d9", "#fcdfee", "#e5f4cf", "#b7e085", "#7ebb40", "#4c9120"], 9 ["#c41a7c", "#dd76ad", "#f0b5d9", "#fcdfee", "#f6f6f6", "#e5f4cf", "#b7e085", "#7ebb40", "#4c9120"], 10 ["#8d0051", "#c41a7c", "#dd76ad", "#f0b5d9", "#fcdfee", "#e5f4cf", "#b7e085", "#7ebb40", "#4c9120", "#266318"], 11 ["#8d0051", "#c41a7c", "#dd76ad", "#f0b5d9", "#fcdfee", "#f6f6f6", "#e5f4cf", "#b7e085", "#7ebb40", "#4c9120", "#266318"], :type :div}

   :prgn {3 ["#ae8cc2", "#f6f6f6", "#7ebe7a"], 4 ["#7a3193", "#c1a4ce", "#a5da9f", "#008736"], 5 ["#7a3193", "#c1a4ce", "#f6f6f6", "#a5da9f", "#008736"], 6 ["#752982", "#ae8cc2", "#e6d3e7", "#d8efd2", "#7ebe7a", "#1a7736"], 7 ["#752982", "#ae8cc2", "#e6d3e7", "#f6f6f6", "#d8efd2", "#7ebe7a", "#1a7736"], 8 ["#752982", "#986faa", "#c1a4ce", "#e6d3e7", "#d8efd2", "#a5da9f", "#59ad60", "#1a7736"], 9 ["#752982", "#986faa", "#c1a4ce", "#e6d3e7", "#f6f6f6", "#d8efd2", "#a5da9f", "#59ad60", "#1a7736"], 10 ["#3f004a", "#752982", "#986faa", "#c1a4ce", "#e6d3e7", "#d8efd2", "#a5da9f", "#59ad60", "#1a7736", "#00431a"], 11 ["#3f004a", "#752982", "#986faa", "#c1a4ce", "#e6d3e7", "#f6f6f6", "#d8efd2", "#a5da9f", "#59ad60", "#1a7736", "#00431a"], :type :div}

   :rdylbu {3 ["#fb8c58", "#fefebe", "#90beda"], 4 ["#d6181b", "#fcad60", "#aad8e8", "#2b7ab5"], 5 ["#d6181b", "#fcad60", "#fefebe", "#aad8e8", "#2b7ab5"], 6 ["#d62f26", "#fb8c58", "#fddf8f", "#dff2f7", "#90beda", "#4474b3"], 7 ["#d62f26", "#fb8c58", "#fddf8f", "#fefebe", "#dff2f7", "#90beda", "#4474b3"], 8 ["#d62f26", "#f36c42", "#fcad60", "#fddf8f", "#dff2f7", "#aad8e8", "#73acd0", "#4474b3"], 9 ["#d62f26", "#f36c42", "#fcad60", "#fddf8f", "#fefebe", "#dff2f7", "#aad8e8", "#73acd0", "#4474b3"], 10 ["#a40025", "#d62f26", "#f36c42", "#fcad60", "#fddf8f", "#dff2f7", "#aad8e8", "#73acd0", "#4474b3", "#303594"], 11 ["#a40025", "#d62f26", "#f36c42", "#fcad60", "#fddf8f", "#fefebe", "#dff2f7", "#aad8e8", "#73acd0", "#4474b3", "#303594"], :type :div}

   :brbg {3 ["#d7b264", "#f4f4f4", "#59b3ab"], 4 ["#a56019", "#dec17c", "#7fccc0", "#008470"], 5 ["#a56019", "#dec17c", "#f4f4f4", "#7fccc0", "#008470"], 6 ["#8b5009", "#d7b264", "#f5e7c2", "#c6e9e4", "#59b3ab", "#00655d"], 7 ["#8b5009", "#d7b264", "#f5e7c2", "#f4f4f4", "#c6e9e4", "#59b3ab", "#00655d"], 8 ["#8b5009", "#be802c", "#dec17c", "#f5e7c2", "#c6e9e4", "#7fccc0", "#34968e", "#00655d"], 9 ["#8b5009", "#be802c", "#dec17c", "#f5e7c2", "#f4f4f4", "#c6e9e4", "#7fccc0", "#34968e", "#00655d"], 10 ["#532f04", "#8b5009", "#be802c", "#dec17c", "#f5e7c2", "#c6e9e4", "#7fccc0", "#34968e", "#00655d", "#003b2f"], 11 ["#532f04", "#8b5009", "#be802c", "#dec17c", "#f5e7c2", "#f4f4f4", "#c6e9e4", "#7fccc0", "#34968e", "#00655d", "#003b2f"], :type :div}

   :rdgy {3 ["#ee8961", "#fefefe", "#989898"], 4 ["#c9001f", "#f3a481", "#b9b9b9", "#3f3f3f"], 5 ["#c9001f", "#f3a481", "#fefefe", "#b9b9b9", "#3f3f3f"], 6 ["#b1172a", "#ee8961", "#fcdac6", "#dfdfdf", "#989898", "#4c4c4c"], 7 ["#b1172a", "#ee8961", "#fcdac6", "#fefefe", "#dfdfdf", "#989898", "#4c4c4c"], 8 ["#b1172a", "#d55f4c", "#f3a481", "#fcdac6", "#dfdfdf", "#b9b9b9", "#868686", "#4c4c4c"], 9 ["#b1172a", "#d55f4c", "#f3a481", "#fcdac6", "#fefefe", "#dfdfdf", "#b9b9b9", "#868686", "#4c4c4c"], 10 ["#66001e", "#b1172a", "#d55f4c", "#f3a481", "#fcdac6", "#dfdfdf", "#b9b9b9", "#868686", "#4c4c4c", "#191919"], 11 ["#66001e", "#b1172a", "#d55f4c", "#f3a481", "#fcdac6", "#fefefe", "#dfdfdf", "#b9b9b9", "#868686", "#4c4c4c", "#191919"], :type :div}

   :puor {3 ["#f0a23f", "#f6f6f6", "#988dc2"], 4 ["#e56000", "#fcb762", "#b1aad1", "#5d3b98"], 5 ["#e56000", "#fcb762", "#f6f6f6", "#b1aad1", "#5d3b98"], 6 ["#b25705", "#f0a23f", "#fddfb5", "#d7d9ea", "#988dc2", "#532687"], 7 ["#b25705", "#f0a23f", "#fddfb5", "#f6f6f6", "#d7d9ea", "#988dc2", "#532687"], 8 ["#b25705", "#df8113", "#fcb762", "#fddfb5", "#d7d9ea", "#b1aad1", "#7f72ab", "#532687"], 9 ["#b25705", "#df8113", "#fcb762", "#fddfb5", "#f6f6f6", "#d7d9ea", "#b1aad1", "#7f72ab", "#532687"], 10 ["#7e3a07", "#b25705", "#df8113", "#fcb762", "#fddfb5", "#d7d9ea", "#b1aad1", "#7f72ab", "#532687", "#2c004a"], 11 ["#7e3a07", "#b25705", "#df8113", "#fcb762", "#fddfb5", "#f6f6f6", "#d7d9ea", "#b1aad1", "#7f72ab", "#532687", "#2c004a"], :type :div}

   :set2 {3 ["#65c1a4", "#fb8c61", "#8c9fca"], 4 ["#65c1a4", "#fb8c61", "#8c9fca", "#e689c2"], 5 ["#65c1a4", "#fb8c61", "#8c9fca", "#e689c2", "#a5d753"], 6 ["#65c1a4", "#fb8c61", "#8c9fca", "#e689c2", "#a5d753", "#fed82e"], 7 ["#65c1a4", "#fb8c61", "#8c9fca", "#e689c2", "#a5d753", "#fed82e", "#e4c393"], 8 ["#65c1a4", "#fb8c61", "#8c9fca", "#e689c2", "#a5d753", "#fed82e", "#e4c393", "#b2b2b2"], :type :qual}

   :accent {3 ["#7ec87e", "#bdadd3", "#fcbf85"], 4 ["#7ec87e", "#bdadd3", "#fcbf85", "#fefe98"], 5 ["#7ec87e", "#bdadd3", "#fcbf85", "#fefe98", "#376baf"], 6 ["#7ec87e", "#bdadd3", "#fcbf85", "#fefe98", "#376baf", "#ef017e"], 7 ["#7ec87e", "#bdadd3", "#fcbf85", "#fefe98", "#376baf", "#ef017e", "#be5a16"], 8 ["#7ec87e", "#bdadd3", "#fcbf85", "#fefe98", "#376baf", "#ef017e", "#be5a16", "#656565"], :type :qual}

   :set1 {3 ["#e3191b", "#367db7", "#4cae49"], 4 ["#e3191b", "#367db7", "#4cae49", "#974da2"], 5 ["#e3191b", "#367db7", "#4cae49", "#974da2", "#fe7e00"], 6 ["#e3191b", "#367db7", "#4cae49", "#974da2", "#fe7e00", "#fefe32"], 7 ["#e3191b", "#367db7", "#4cae49", "#974da2", "#fe7e00", "#fefe32", "#a55527"], 8 ["#e3191b", "#367db7", "#4cae49", "#974da2", "#fe7e00", "#fefe32", "#a55527", "#f680be"], 9 ["#e3191b", "#367db7", "#4cae49", "#974da2", "#fe7e00", "#fefe32", "#a55527", "#f680be", "#989898"], :type :qual}

   :set3 {3 ["#8cd2c6", "#fefeb2", "#bdb9d9"], 4 ["#8cd2c6", "#fefeb2", "#bdb9d9", "#fa7f71"], 5 ["#8cd2c6", "#fefeb2", "#bdb9d9", "#fa7f71", "#7fb0d2"], 6 ["#8cd2c6", "#fefeb2", "#bdb9d9", "#fa7f71", "#7fb0d2", "#fcb361"], 7 ["#8cd2c6", "#fefeb2", "#bdb9d9", "#fa7f71", "#7fb0d2", "#fcb361", "#b2dd68"], 8 ["#8cd2c6", "#fefeb2", "#bdb9d9", "#fa7f71", "#7fb0d2", "#fcb361", "#b2dd68", "#fbcce4"], 9 ["#8cd2c6", "#fefeb2", "#bdb9d9", "#fa7f71", "#7fb0d2", "#fcb361", "#b2dd68", "#fbcce4", "#d8d8d8"], 10 ["#8cd2c6", "#fefeb2", "#bdb9d9", "#fa7f71", "#7fb0d2", "#fcb361", "#b2dd68", "#fbcce4", "#d8d8d8", "#bb7fbc"], 11 ["#8cd2c6", "#fefeb2", "#bdb9d9", "#fa7f71", "#7fb0d2", "#fcb361", "#b2dd68", "#fbcce4", "#d8d8d8", "#bb7fbc", "#cbeac4"], 12 ["#8cd2c6", "#fefeb2", "#bdb9d9", "#fa7f71", "#7fb0d2", "#fcb361", "#b2dd68", "#fbcce4", "#d8d8d8", "#bb7fbc", "#cbeac4", "#feec6e"], :type :qual}

   :dark2 {3 ["#1a9d76", "#d85e01", "#746fb2"], 4 ["#1a9d76", "#d85e01", "#746fb2", "#e62889"], 5 ["#1a9d76", "#d85e01", "#746fb2", "#e62889", "#65a51d"], 6 ["#1a9d76", "#d85e01", "#746fb2", "#e62889", "#65a51d", "#e5aa01"], 7 ["#1a9d76", "#d85e01", "#746fb2", "#e62889", "#65a51d", "#e5aa01", "#a5751c"], 8 ["#1a9d76", "#d85e01", "#746fb2", "#e62889", "#65a51d", "#e5aa01", "#a5751c", "#656565"], :type :qual}

   :paired {3 ["#a5cde2", "#1e77b3", "#b1de89"], 4 ["#a5cde2", "#1e77b3", "#b1de89", "#329f2b"], 5 ["#a5cde2", "#1e77b3", "#b1de89", "#329f2b", "#fa9998"], 6 ["#a5cde2", "#1e77b3", "#b1de89", "#329f2b", "#fa9998", "#e2191b"], 7 ["#a5cde2", "#1e77b3", "#b1de89", "#329f2b", "#fa9998", "#e2191b", "#fcbe6e"], 8 ["#a5cde2", "#1e77b3", "#b1de89", "#329f2b", "#fa9998", "#e2191b", "#fcbe6e", "#fe7e00"], 9 ["#a5cde2", "#1e77b3", "#b1de89", "#329f2b", "#fa9998", "#e2191b", "#fcbe6e", "#fe7e00", "#c9b1d5"], 10 ["#a5cde2", "#1e77b3", "#b1de89", "#329f2b", "#fa9998", "#e2191b", "#fcbe6e", "#fe7e00", "#c9b1d5", "#693c99"], 11 ["#a5cde2", "#1e77b3", "#b1de89", "#329f2b", "#fa9998", "#e2191b", "#fcbe6e", "#fe7e00", "#c9b1d5", "#693c99", "#fefe98"], 12 ["#a5cde2", "#1e77b3", "#b1de89", "#329f2b", "#fa9998", "#e2191b", "#fcbe6e", "#fe7e00", "#c9b1d5", "#693c99", "#fefe98", "#b05827"], :type :qual}

   :pastel2 {3 ["#b2e1cc", "#fcccab", "#cad4e7"], 4 ["#b2e1cc", "#fcccab", "#cad4e7", "#f3c9e3"], 5 ["#b2e1cc", "#fcccab", "#cad4e7", "#f3c9e3", "#e5f4c8"], 6 ["#b2e1cc", "#fcccab", "#cad4e7", "#f3c9e3", "#e5f4c8", "#fef1ad"], 7 ["#b2e1cc", "#fcccab", "#cad4e7", "#f3c9e3", "#e5f4c8", "#fef1ad", "#f0e1cb"], 8 ["#b2e1cc", "#fcccab", "#cad4e7", "#f3c9e3", "#e5f4c8", "#fef1ad", "#f0e1cb", "#cbcbcb"], :type :qual}

   :pastel1 {3 ["#fab3ad", "#b2cce2", "#cbeac4"], 4 ["#fab3ad", "#b2cce2", "#cbeac4", "#ddcae3"], 5 ["#fab3ad", "#b2cce2", "#cbeac4", "#ddcae3", "#fdd8a5"], 6 ["#fab3ad", "#b2cce2", "#cbeac4", "#ddcae3", "#fdd8a5", "#fefecb"], 7 ["#fab3ad", "#b2cce2", "#cbeac4", "#ddcae3", "#fdd8a5", "#fefecb", "#e4d7bc"], 8 ["#fab3ad", "#b2cce2", "#cbeac4", "#ddcae3", "#fdd8a5", "#fefecb", "#e4d7bc", "#fcd9eb"], 9 ["#fab3ad", "#b2cce2", "#cbeac4", "#ddcae3", "#fdd8a5", "#fefecb", "#e4d7bc", "#fcd9eb", "#f1f1f1"], :type :qual}

   :orrd {3 ["#fde7c7", "#fcba83", "#e24932"], 4 ["#fdefd8", "#fccb89", "#fb8c58", "#d62f1e"], 5 ["#fdefd8", "#fccb89", "#fb8c58", "#e24932", "#b20000"], 6 ["#fdefd8", "#fcd39d", "#fcba83", "#fb8c58", "#e24932", "#b20000"], 7 ["#fdefd8", "#fcd39d", "#fcba83", "#fb8c58", "#ee6447", "#d62f1e", "#980000"], 8 ["#fef6eb", "#fde7c7", "#fcd39d", "#fcba83", "#fb8c58", "#ee6447", "#d62f1e", "#980000"], 9 ["#fef6eb", "#fde7c7", "#fcd39d", "#fcba83", "#fb8c58", "#ee6447", "#d62f1e", "#b20000", "#7e0000"], :type :seq}

   :pubu {3 ["#ebe6f1", "#a5bcda", "#2a8bbd"], 4 ["#f0edf5", "#bcc8e0", "#73a8ce", "#046faf"], 5 ["#f0edf5", "#bcc8e0", "#73a8ce", "#2a8bbd", "#03598c"], 6 ["#f0edf5", "#cfd0e5", "#a5bcda", "#73a8ce", "#2a8bbd", "#03598c"], 7 ["#f0edf5", "#cfd0e5", "#a5bcda", "#73a8ce", "#358fbf", "#046faf", "#024d7a"], 8 ["#fef6fa", "#ebe6f1", "#cfd0e5", "#a5bcda", "#73a8ce", "#358fbf", "#046faf", "#024d7a"], 9 ["#fef6fa", "#ebe6f1", "#cfd0e5", "#a5bcda", "#73a8ce", "#358fbf", "#046faf", "#03598c", "#013757"], :type :seq}

   :bupu {3 ["#dfebf3", "#9dbbd9", "#8755a6"], 4 ["#ecf7fa", "#b2cce2", "#8b95c5", "#87409c"], 5 ["#ecf7fa", "#b2cce2", "#8b95c5", "#8755a6", "#800e7b"], 6 ["#ecf7fa", "#bed2e5", "#9dbbd9", "#8b95c5", "#8755a6", "#800e7b"], 7 ["#ecf7fa", "#bed2e5", "#9dbbd9", "#8b95c5", "#8b6ab0", "#87409c", "#6d006a"], 8 ["#f6fbfc", "#dfebf3", "#bed2e5", "#9dbbd9", "#8b95c5", "#8b6ab0", "#87409c", "#6d006a"], 9 ["#f6fbfc", "#dfebf3", "#bed2e5", "#9dbbd9", "#8b95c5", "#8b6ab0", "#87409c", "#800e7b", "#4c004a"], :type :seq}

   :oranges {3 ["#fde5cd", "#fcad6a", "#e5540c"], 4 ["#fdecdd", "#fcbd84", "#fc8c3b", "#d84600"], 5 ["#fdecdd", "#fcbd84", "#fc8c3b", "#e5540c", "#a53502"], 6 ["#fdecdd", "#fccfa1", "#fcad6a", "#fc8c3b", "#e5540c", "#a53502"], 7 ["#fdecdd", "#fccfa1", "#fcad6a", "#fc8c3b", "#f06812", "#d84700", "#8b2c03"], 8 ["#fef4ea", "#fde5cd", "#fccfa1", "#fcad6a", "#fc8c3b", "#f06812", "#d84700", "#8b2c03"], 9 ["#fef4ea", "#fde5cd", "#fccfa1", "#fcad6a", "#fc8c3b", "#f06812", "#d84700", "#a53502", "#7e2603"], :type :seq}

   :bugn {3 ["#e4f4f8", "#98d7c8", "#2ba15e"], 4 ["#ecf7fa", "#b1e1e1", "#65c1a3", "#228a44"], 5 ["#ecf7fa", "#b1e1e1", "#65c1a3", "#2ba15e", "#006c2b"], 6 ["#ecf7fa", "#cbebe5", "#98d7c8", "#65c1a3", "#2ba15e", "#006c2b"], 7 ["#ecf7fa", "#cbebe5", "#98d7c8", "#65c1a3", "#40ad75", "#228a44", "#005723"], 8 ["#f6fbfc", "#e4f4f8", "#cbebe5", "#98d7c8", "#65c1a3", "#40ad75", "#228a44", "#005723"], 9 ["#f6fbfc", "#e4f4f8", "#cbebe5", "#98d7c8", "#65c1a3", "#40ad75", "#228a44", "#006c2b", "#00431a"], :type :seq}

   :ylorbr {3 ["#fef6bb", "#fdc34e", "#d85e0d"], 4 ["#fefed3", "#fdd88d", "#fd9828", "#cb4b01"], 5 ["#fefed3", "#fdd88d", "#fd9828", "#d85e0d", "#983303"], 6 ["#fefed3", "#fde290", "#fdc34e", "#fd9828", "#d85e0d", "#983303"], 7 ["#fefed3", "#fde290", "#fdc34e", "#fd9828", "#eb6f13", "#cb4b01", "#8b2c03"], 8 ["#fefee4", "#fef6bb", "#fde290", "#fdc34e", "#fd9828", "#eb6f13", "#cb4b01", "#8b2c03"], 9 ["#fefee4", "#fef6bb", "#fde290", "#fdc34e", "#fd9828", "#eb6f13", "#cb4b01", "#983303", "#652405"], :type :seq}

   :ylgn {3 ["#f6fbb8", "#acdc8d", "#30a253"], 4 ["#fefecb", "#c1e598", "#77c578", "#228342"], 5 ["#fefecb", "#c1e598", "#77c578", "#30a253", "#006736"], 6 ["#fefecb", "#d8efa2", "#acdc8d", "#77c578", "#30a253", "#006736"], 7 ["#fefecb", "#d8efa2", "#acdc8d", "#77c578", "#40aa5c", "#228342", "#005931"], 8 ["#fefee4", "#f6fbb8", "#d8efa2", "#acdc8d", "#77c578", "#40aa5c", "#228342", "#005931"], 9 ["#fefee4", "#f6fbb8", "#d8efa2", "#acdc8d", "#77c578", "#40aa5c", "#228342", "#006736", "#004428"], :type :seq}

   :reds {3 ["#fddfd1", "#fb9171", "#dd2c25"], 4 ["#fde4d8", "#fbad90", "#fa6949", "#ca171c"], 5 ["#fde4d8", "#fbad90", "#fa6949", "#dd2c25", "#a40e14"], 6 ["#fde4d8", "#fbbaa0", "#fb9171", "#fa6949", "#dd2c25", "#a40e14"], 7 ["#fde4d8", "#fbbaa0", "#fb9171", "#fa6949", "#ee3a2b", "#ca171c", "#98000c"], 8 ["#fef4ef", "#fddfd1", "#fbbaa0", "#fb9171", "#fa6949", "#ee3a2b", "#ca171c", "#98000c"], 9 ["#fef4ef", "#fddfd1", "#fbbaa0", "#fb9171", "#fa6949", "#ee3a2b", "#ca171c", "#a40e14", "#66000c"], :type :seq}

   :rdpu {3 ["#fcdfdc", "#f99eb4", "#c41a89"], 4 ["#fdeae1", "#fab3b8", "#f667a0", "#ad007d"], 5 ["#fdeae1", "#fab3b8", "#f667a0", "#c41a89", "#790076"], 6 ["#fdeae1", "#fbc4bf", "#f99eb4", "#f667a0", "#c41a89", "#790076"], 7 ["#fdeae1", "#fbc4bf", "#f99eb4", "#f667a0", "#dc3396", "#ad007d", "#790076"], 8 ["#fef6f2", "#fcdfdc", "#fbc4bf", "#f99eb4", "#f667a0", "#dc3396", "#ad007d", "#790076"], 9 ["#fef6f2", "#fcdfdc", "#fbc4bf", "#f99eb4", "#f667a0", "#dc3396", "#ad007d", "#790076", "#480069"], :type :seq}

   :greens {3 ["#e4f4df", "#a0d89a", "#30a253"], 4 ["#ecf7e8", "#b9e3b2", "#73c375", "#228a44"], 5 ["#ecf7e8", "#b9e3b2", "#73c375", "#30a253", "#006c2b"], 6 ["#ecf7e8", "#c6e8bf", "#a0d89a", "#73c375", "#30a253", "#006c2b"], 7 ["#ecf7e8", "#c6e8bf", "#a0d89a", "#73c375", "#40aa5c", "#228a44", "#005931"], 8 ["#f6fbf4", "#e4f4df", "#c6e8bf", "#a0d89a", "#73c375", "#40aa5c", "#228a44", "#005931"], 9 ["#f6fbf4", "#e4f4df", "#c6e8bf", "#a0d89a", "#73c375", "#40aa5c", "#228a44", "#006c2b", "#00431a"], :type :seq}

   :ylgnbu {3 ["#ecf7b0", "#7eccba", "#2b7eb7"], 4 ["#fefecb", "#a0d9b3", "#40b5c3", "#215da7"], 5 ["#fefecb", "#a0d9b3", "#40b5c3", "#2b7eb7", "#243393"], 6 ["#fefecb", "#c6e8b3", "#7eccba", "#40b5c3", "#2b7eb7", "#243393"], 7 ["#fefecb", "#c6e8b3", "#7eccba", "#40b5c3", "#1c90bf", "#215da7", "#0b2b83"], 8 ["#fefed8", "#ecf7b0", "#c6e8b3", "#7eccba", "#40b5c3", "#1c90bf", "#215da7", "#0b2b83"], 9 ["#fefed8", "#ecf7b0", "#c6e8b3", "#7eccba", "#40b5c3", "#1c90bf", "#215da7", "#243393", "#071c57"], :type :seq}

   :purples {3 ["#eeecf4", "#bbbcdb", "#746ab0"], 4 ["#f1eff6", "#cac8e1", "#9d99c7", "#6950a2"], 5 ["#f1eff6", "#cac8e1", "#9d99c7", "#746ab0", "#53268e"], 6 ["#f1eff6", "#d9d9ea", "#bbbcdb", "#9d99c7", "#746ab0", "#53268e"], 7 ["#f1eff6", "#d9d9ea", "#bbbcdb", "#9d99c7", "#7f7cb9", "#6950a2", "#491385"], 8 ["#fbfafc", "#eeecf4", "#d9d9ea", "#bbbcdb", "#9d99c7", "#7f7cb9", "#6950a2", "#491385"], 9 ["#fbfafc", "#eeecf4", "#d9d9ea", "#bbbcdb", "#9d99c7", "#7f7cb9", "#6950a2", "#53268e", "#3e007c"], :type :seq}

   :gnbu {3 ["#dff2da", "#a7dcb4", "#42a1c9"], 4 ["#eff8e7", "#b9e3bb", "#7acbc3", "#2a8bbd"], 5 ["#eff8e7", "#b9e3bb", "#7acbc3", "#42a1c9", "#0767ab"], 6 ["#eff8e7", "#cbeac4", "#a7dcb4", "#7acbc3", "#42a1c9", "#0767ab"], 7 ["#eff8e7", "#cbeac4", "#a7dcb4", "#7acbc3", "#4db2d2", "#2a8bbd", "#07579d"], 8 ["#f6fbef", "#dff2da", "#cbeac4", "#a7dcb4", "#7acbc3", "#4db2d2", "#2a8bbd", "#07579d"], 9 ["#f6fbef", "#dff2da", "#cbeac4", "#a7dcb4", "#7acbc3", "#4db2d2", "#2a8bbd", "#0767ab", "#073f80"], :type :seq}

   :greys {3 ["#efefef", "#bcbcbc", "#626262"], 4 ["#f6f6f6", "#cbcbcb", "#959595", "#515151"], 5 ["#f6f6f6", "#cbcbcb", "#959595", "#626262", "#242424"], 6 ["#f6f6f6", "#d8d8d8", "#bcbcbc", "#959595", "#626262", "#242424"], 7 ["#f6f6f6", "#d8d8d8", "#bcbcbc", "#959595", "#727272", "#515151", "#242424"], 8 ["#fefefe", "#efefef", "#d8d8d8", "#bcbcbc", "#959595", "#727272", "#515151", "#242424"], 9 ["#fefefe", "#efefef", "#d8d8d8", "#bcbcbc", "#959595", "#727272", "#515151", "#242424", "#000000"], :type :seq}

   :ylorrd {3 ["#feec9f", "#fdb14b", "#ef3a1f"], 4 ["#fefeb1", "#fdcb5b", "#fc8c3b", "#e2191b"], 5 ["#fefeb1", "#fdcb5b", "#fc8c3b", "#ef3a1f", "#bc0025"], 6 ["#fefeb1", "#fdd875", "#fdb14b", "#fc8c3b", "#ef3a1f", "#bc0025"], 7 ["#fefeb1", "#fdd875", "#fdb14b", "#fc8c3b", "#fb4d29", "#e2191b", "#b00025"], 8 ["#fefecb", "#feec9f", "#fdd875", "#fdb14b", "#fc8c3b", "#fb4d29", "#e2191b", "#b00025"], :type :seq}

   :purd {3 ["#e6e0ee", "#c893c6", "#dc1b76"], 4 ["#f0edf5", "#d6b4d7", "#de64af", "#cd1155"], 5 ["#f0edf5", "#d6b4d7", "#de64af", "#dc1b76", "#970042"], 6 ["#f0edf5", "#d3b8d9", "#c893c6", "#de64af", "#dc1b76", "#970042"], 7 ["#f0edf5", "#d3b8d9", "#c893c6", "#de64af", "#e62889", "#cd1155", "#90003e"], 8 ["#f6f3f8", "#e6e0ee", "#d3b8d9", "#c893c6", "#de64af", "#e62889", "#cd1155", "#90003e"], 9 ["#f6f3f8", "#e6e0ee", "#d3b8d9", "#c893c6", "#de64af", "#e62889", "#cd1155", "#970042", "#66001e"], :type :seq}

   :blues {3 ["#ddeaf6", "#9dc9e0", "#3081bc"], 4 ["#eef2fe", "#bcd6e6", "#6aadd5", "#2070b4"], 5 ["#eef2fe", "#bcd6e6", "#6aadd5", "#3081bc", "#07509b"], 6 ["#eef2fe", "#c5daee", "#9dc9e0", "#6aadd5", "#3081bc", "#07509b"], 7 ["#eef2fe", "#c5daee", "#9dc9e0", "#6aadd5", "#4191c5", "#2070b4", "#074493"], 8 ["#f6fafe", "#ddeaf6", "#c5daee", "#9dc9e0", "#6aadd5", "#4191c5", "#2070b4", "#074493"], 9 ["#f6fafe", "#ddeaf6", "#c5daee", "#9dc9e0", "#6aadd5", "#4191c5", "#2070b4", "#07509b", "#072f6a"], :type :seq}
   })

(def brewer-index
  (reduce
   (fn [a [name choices]]
     (merge-with concat
      a
      (let [type (:type choices)
            summary (into
                     {}
                     (for [k (keys choices) :when (int? k)]
                       [[type k] [name]]))]
        summary
        )))
   {} brewer-schemes))

(defn choose [n & {:keys [type k] :or {type :qual k 0}}]
  (let [n (max 3 n)
        options (get brewer-index [type n])]
    (-> brewer-schemes (get (nth (sort options) k)) (get n))))

(defn for-set [values & {:keys [k] :or {k 0}}]
  (let [color-set (choose (count values) :k k)]
    (into {} (map vector values color-set))))
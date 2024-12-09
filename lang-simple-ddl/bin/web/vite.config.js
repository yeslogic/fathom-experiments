// https://github.com/nojaf/vite-plugin-fable
// https://github.com/pdelacroix/vite-plugin-melange

import dunePlugin from 'vite-plugin-dune';

export default {
  build: {
    outDir: './dist',
  },
  plugins: [
    dunePlugin(),
  ]
};

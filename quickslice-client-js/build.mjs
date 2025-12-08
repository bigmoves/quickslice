import * as esbuild from 'esbuild';

const watch = process.argv.includes('--watch');

const sharedConfig = {
  entryPoints: ['src/index.ts'],
  bundle: true,
  sourcemap: true,
  target: ['es2020'],
};

// UMD build (for CDN/script tag)
const umdBuild = {
  ...sharedConfig,
  outfile: 'dist/quickslice-client.js',
  format: 'iife',
  globalName: 'QuicksliceClient',
};

// UMD minified
const umdMinBuild = {
  ...sharedConfig,
  outfile: 'dist/quickslice-client.min.js',
  format: 'iife',
  globalName: 'QuicksliceClient',
  minify: true,
  sourcemap: false,
};

// ESM build (for bundlers)
const esmBuild = {
  ...sharedConfig,
  outfile: 'dist/quickslice-client.esm.js',
  format: 'esm',
};

async function build() {
  if (watch) {
    const ctx = await esbuild.context(umdBuild);
    await ctx.watch();
    console.log('Watching for changes...');
  } else {
    await Promise.all([
      esbuild.build(umdBuild),
      esbuild.build(umdMinBuild),
      esbuild.build(esmBuild),
    ]);
    console.log('Build complete!');
  }
}

build().catch((err) => {
  console.error(err);
  process.exit(1);
});

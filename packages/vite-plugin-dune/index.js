import fs from 'fs/promises';
import path from 'path';
import process from 'process';
import { execSync, spawn, spawnSync } from 'child_process';

function parseDuneWorkspace(workspace) {
  const [, rootDir] = workspace.match(/\(root (?<path>.+)\)/);
  const [, buildContext] = workspace.match(/\(build_context (?<path>.+)\)/);
  return {
    // The absolute path to the root of the dune workspace
    rootDir,

    // The absolute path of the build directory
    buildDir: path.resolve(rootDir, buildContext),

    // Given an absolute path to an `.ml`, this returns the path to the
    // `.bs.js` file that `js_of_ocaml` would compile it to.
    ocamlToJsPath(ocamlPath) {
      const relativeDir = path.dirname(path.relative(this.rootDir, ocamlPath)); // relative directory of `.ml` file
      const fileName = path.basename(ocamlPath, '.ml').concat('.bc.js');        // name of compiled `.bs.js` file
      return path.resolve(this.buildDir, relativeDir, fileName);                // absolute path to `.bs.js` file
    }
  };
}

export default function ocamlPlugin() {
  let state = {
    duneWorkspace: null,
    duneProcess: null,
  };

  return {
    name: 'dune',
    enforce: 'pre',

    // https://rollupjs.org/plugin-development/#buildstart
    async buildStart() {
      // Collect information
      state.duneWorkspace = parseDuneWorkspace(execSync(`dune show workspace --no-print-directory`).toString());

      if (this.meta.watchMode) {
        // Start watching the OCaml files for changes
        state.duneProcess = spawn('dune', ['build', '--watch', `--root=${state.duneWorkspace.rootDir}`]);

        let error = '';

        state.duneProcess.stderr.on('data', data => {
          error += data.toString();
        });

        state.duneProcess.on('close', code => {
          if (code !== 0 && error !== '') {
            this.error(`child process exited with code ${code}`);
            this.error('\n' + error);
          }
        });

        process.on('exit', () => {
          this.debug("killing dune process");
          state.duneProcess.kill();
        });
      } else {
        // Build the project in release mode, which results in a smaller file size
        const result = spawnSync('dune', ['build', '--profile=release', `--root=${state.duneWorkspace.rootDir}`]);

        if (result.status !== 0 && result.stderr) {
          this.error('\n' + result.stderr.toString());
        }
      }
    },

    // https://rollupjs.org/plugin-development/#closebundle
    async closeBundle() {
      state.duneProcess?.duneProcess.kill();
    },

    // https://rollupjs.org/plugin-development/#load
    async load(id) {
      if (id.endsWith('.ml')) {
        const compiledFilePath = state.duneWorkspace.ocamlToJsPath(id);

        this.addWatchFile(compiledFilePath);

        return {
          code: await fs.readFile(compiledFilePath, 'utf8'),
        };
      }
    },
  };
}

import fs from 'fs/promises';
import path from 'path';
import process from 'process';
import { execSync, spawn, spawnSync } from 'child_process';

function parseDuneWorkspace(workspace) {
  return {
    root: workspace.match(/\(root (?<path>.+)\)/)[1],
    buildContext: workspace.match(/\(build_context (?<path>.+)\)/)[1],
  };
}

export default function ocamlPlugin() {
  let state = {
    duneWorkspace: null,
    duneProcess: null,
  };

  function ocamlToJsPath(ocamlPath) {
    const relativePath = path.dirname(path.relative(state.duneWorkspace.root, ocamlPath)); // '/lang-simple-ddl/bin/web/'
    const name = path.basename(ocamlPath, '.ml') // 'index'
    return path.join(state.duneWorkspace.root, state.duneWorkspace.buildContext, relativePath, `${name}.bc.js`);
  }

  return {
    name: 'dune',
    enforce: 'pre',

    // https://rollupjs.org/plugin-development/#buildstart
    async buildStart() {
      // Collect information
      state.duneWorkspace = parseDuneWorkspace(execSync(`dune show workspace --no-print-directory`).toString());

      if (this.meta.watchMode) {
        // Start watching the OCaml files for changes
        state.duneProcess = spawn('dune', ['build', '--watch', `--root=${state.duneWorkspace.root}`]);

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
        const result = spawnSync('dune', ['build', '--profile=release', `--root=${state.duneWorkspace.root}`]);

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
        const jsFilePath = ocamlToJsPath(id);

        this.addWatchFile(jsFilePath);

        return {
          code: await fs.readFile(jsFilePath, 'utf8'),
        };
      }
    },
  };
}

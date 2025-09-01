const { elabProgram, compileProgram } = driver;

function getExample(name) {
  return examples.all.find((example) => example.name === name).source;
}

customElements.define('my-application', class extends HTMLElement {
  #toolbar;
  #editorPane;
  #outputPane;

  connectedCallback() {
    this.id = "main";

    this.querySelector('noscript').remove();

    // https://stackoverflow.com/a/55027467
    this.#toolbar = document.createElement('my-toolbar');
    this.#editorPane = document.createElement('my-editor-pane');
    this.#outputPane = document.createElement('my-output-pane');

    this.#toolbar.addEventListener("selectExample", this.#handleSelectExample.bind(this));
    this.#toolbar.addEventListener("elaborate", this.#handleElaborate.bind(this));
    this.#toolbar.addEventListener("compile", this.#handleCompile.bind(this));

    this.replaceChildren(
      this.#toolbar,
      this.#editorPane,
      this.#outputPane
    );
  }

  #handleSelectExample(event) {
    this.#editorPane.source = getExample(event.detail);
  }

  #handleElaborate() {
    this.#outputPane.contents = elabProgram(this.#editorPane.source);
  }

  #handleCompile() {
    this.#outputPane.contents = compileProgram(this.#editorPane.source);
  }
});

customElements.define('my-toolbar', class extends HTMLElement {
  connectedCallback() {
    this.id = "toolbar";

    const exampleSelect = document.createElement("select");
    examples.all.forEach(({ name }) => {
      const option = document.createElement('option');
      option.value = name;
      option.textContent = name;
      option.selected = name === examples.initial;
      exampleSelect.appendChild(option);
    });
    exampleSelect.addEventListener("input", (event) => {
      event.preventDefault();
      this.dispatchEvent(new CustomEvent("selectExample", {
        detail: exampleSelect.value
      }));
    });

    const elabButton = document.createElement("button");
    elabButton.textContent = "Elaborate";
    elabButton.addEventListener("click", (event) => {
      event.preventDefault();
      this.dispatchEvent(new CustomEvent("elaborate"));
    });

    const compileButton = document.createElement("button");
    compileButton.textContent = "Compile";
    compileButton.addEventListener("click", (event) => {
      event.preventDefault();
      this.dispatchEvent(new CustomEvent("compile"));
    });

    this.replaceChildren(
      exampleSelect,
      elabButton,
      compileButton
    );
  }
});

customElements.define('my-editor-pane', class extends HTMLElement {
  #editorInput;

  connectedCallback() {
    this.id = "editor";
    this.#editorInput = this.appendChild(document.createElement('textarea'));
    this.#editorInput.wrap = "off";
    this.#editorInput.spellcheck = false;
    this.#editorInput.textContent = getExample(examples.initial);
  }

  set source(contents) {
    this.#editorInput.value = contents;
  }

  get source() {
    return this.#editorInput.value;
  }
});

customElements.define('my-output-pane', class extends HTMLElement {
  #outputDisplay;

  connectedCallback() {
    this.id = "output";
    this.#outputDisplay = this.appendChild(document.createElement('pre'));
  }

  set contents(contents) {
    this.#outputDisplay.textContent = contents;
  }
});

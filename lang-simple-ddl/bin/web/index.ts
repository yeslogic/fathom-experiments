// Global defintions added in `main.ml`
declare const lang: {
  examples: {
    initial: string;
    all: [{ name: string, source: string }];
  },
  driver: {
    elabProgram(source: string): string,
    compileProgram(source: string): string,
  },
};

/**
 * A decorator that registers a class as a custom element. In order to use this,
 * the `name` must be added to the global `HTMLElementTagNameMap` interface.
 */
function customElement(name: keyof HTMLElementTagNameMap): (target: CustomElementConstructor) => void {
  return (target) => {
    window.customElements.define(name, target);
  }
}

// Mark this as an external module to enable modifications to the global scope
export {};

// Add custom elements to the global scope
declare global {
  interface HTMLElementTagNameMap {
    'app-main': Main;
    'app-toolbar': Toolbar;
    'app-editor-pane': EditorPane;
    'app-output-pane': OutputPane;
  }

  interface HTMLElementEventMap {
    'selectExample': CustomEvent;
    'elaborate': CustomEvent;
    'compile': CustomEvent;
  }
}

@customElement('app-main')
class Main extends HTMLElement {
  #toolbar: Toolbar;
  #editorPane: EditorPane;
  #outputPane: OutputPane;

  constructor() {
    super();

    this.id = 'main';

    this.#toolbar = document.createElement('app-toolbar');
    this.#editorPane = document.createElement('app-editor-pane');
    this.#outputPane = document.createElement('app-output-pane');

    this.#toolbar.addEventListener('selectExample', this.#handleSelectExample.bind(this));
    this.#toolbar.addEventListener('elaborate', this.#handleElaborate.bind(this));
    this.#toolbar.addEventListener('compile', this.#handleCompile.bind(this));

    this.replaceChildren(
      this.#toolbar,
      this.#editorPane,
      this.#outputPane,
    );
  }

  #handleSelectExample(event: CustomEvent) {
    this.#editorPane.source = getExample(event.detail);
  }

  #handleElaborate() {
    this.#outputPane.contents = lang.driver.elabProgram(this.#editorPane.source);
  }

  #handleCompile() {
    this.#outputPane.contents = lang.driver.compileProgram(this.#editorPane.source);
  }
}

@customElement('app-toolbar')
class Toolbar extends HTMLElement {
  constructor() {
    super();

    this.id = 'toolbar';

    const exampleSelect = document.createElement('select');
    exampleSelect.replaceChildren(
      ...lang.examples.all.map((example) => {
        const option = document.createElement('option');
        option.value = example.name;
        option.textContent = example.name;
        option.selected = example.name === lang.examples.initial;
        exampleSelect.appendChild(option);
        return option;
      }),
    );
    exampleSelect.addEventListener('input', () => {
      this.dispatchEvent(new CustomEvent('selectExample', {
        detail: exampleSelect.value,
      }));
    });

    const elabButton = document.createElement('button');
    elabButton.textContent = 'Elaborate';
    elabButton.addEventListener('click', () => {
      this.dispatchEvent(new CustomEvent('elaborate'));
    });

    const compileButton = document.createElement('button');
    compileButton.textContent = 'Compile';
    compileButton.addEventListener('click', () => {
      this.dispatchEvent(new CustomEvent('compile'));
    });

    this.replaceChildren(
      exampleSelect,
      elabButton,
      compileButton,
    );
  }
}

@customElement('app-editor-pane')
class EditorPane extends HTMLElement {
  #editorInput: HTMLTextAreaElement;

  constructor() {
    super();
    this.id = 'editor';
    this.#editorInput = this.appendChild(document.createElement('textarea'));
    this.#editorInput.wrap = 'off';
    this.#editorInput.spellcheck = false;
    this.#editorInput.textContent = getExample(lang.examples.initial);
  }

  set source(contents: string) {
    this.#editorInput.value = contents;
  }

  get source(): string {
    return this.#editorInput.value;
  }
}

@customElement('app-output-pane')
class OutputPane extends HTMLElement {
  #outputDisplay: HTMLPreElement;

  constructor() {
    super();
    this.id = 'output';
    this.#outputDisplay = this.appendChild(document.createElement('pre'));
  }

  set contents(contents: string) {
    this.#outputDisplay.textContent = contents;
  }
}

function getExample(name: string): string {
  return lang.examples.all.find((example) => example.name === name)?.source || '';
}

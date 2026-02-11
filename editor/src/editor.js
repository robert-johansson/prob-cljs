import { EditorView, keymap } from "@codemirror/view";
import { EditorState } from "@codemirror/state";
import { defaultKeymap, history, historyKeymap } from "@codemirror/commands";
import { syntaxHighlighting, defaultHighlightStyle } from "@codemirror/language";
import { default_extensions, complete_keymap } from "@nextjournal/clojure-mode";

var theme = EditorView.theme({
  "&": {
    fontSize: "13px",
    fontFamily: "'Menlo', 'Monaco', 'Consolas', monospace",
    backgroundColor: "#f5f5f5"
  },
  "&.cm-focused": {
    outline: "none"
  },
  ".cm-content": {
    padding: "10px 12px",
    lineHeight: "1.45"
  },
  ".cm-gutters": {
    display: "none"
  },
  ".cm-activeLine": {
    backgroundColor: "transparent"
  },
  ".cm-selectionBackground, &.cm-focused .cm-selectionBackground": {
    backgroundColor: "#c8daf5"
  }
});

// Find the top-level form around the cursor position
function topLevelForm(state) {
  var doc = state.doc.toString();
  var pos = state.selection.main.head;
  var depth = 0;
  var start = -1;
  // Scan backwards to find form start
  for (var i = pos - 1; i >= 0; i--) {
    var ch = doc[i];
    if (ch === ')' || ch === ']' || ch === '}') depth++;
    else if (ch === '(' || ch === '[' || ch === '{') {
      if (depth === 0) { start = i; break; }
      depth--;
    }
  }
  if (start === -1) {
    // Try forward
    depth = 0;
    for (var i = pos; i < doc.length; i++) {
      var ch = doc[i];
      if (ch === '(' || ch === '[' || ch === '{') { start = i; break; }
    }
  }
  if (start === -1) return doc.trim();
  // Find matching end
  depth = 0;
  for (var i = start; i < doc.length; i++) {
    var ch = doc[i];
    if (ch === '(' || ch === '[' || ch === '{') depth++;
    else if (ch === ')' || ch === ']' || ch === '}') {
      depth--;
      if (depth === 0) return doc.substring(start, i + 1);
    }
  }
  return doc.substring(start);
}

export function createEditor(parent, code, opts) {
  opts = opts || {};
  var onEval = opts.onEval || function() {};

  var evalForm = function(view) {
    onEval(topLevelForm(view.state));
    return true;
  };

  var evalAll = function(view) {
    onEval(view.state.doc.toString());
    return true;
  };

  var evalKeymap = keymap.of([
    { key: "Mod-Enter", run: evalForm },
    { key: "Shift-Mod-Enter", run: evalAll }
  ]);

  var state = EditorState.create({
    doc: code || "",
    extensions: [
      evalKeymap,
      keymap.of(complete_keymap),
      ...default_extensions,
      history(),
      keymap.of(historyKeymap),
      keymap.of(defaultKeymap),
      syntaxHighlighting(defaultHighlightStyle),
      theme
    ]
  });

  var view = new EditorView({ state: state, parent: parent });

  return {
    view: view,
    getCode: function() { return view.state.doc.toString(); },
    setCode: function(s) {
      view.dispatch({
        changes: { from: 0, to: view.state.doc.length, insert: s }
      });
    }
  };
}

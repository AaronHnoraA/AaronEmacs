import { renderInfoview } from '@leanprover/infoview'
import type { InitializeResult } from 'vscode-languageserver-protocol'
import { createEditorApi } from './editor-api'

const editorApi = createEditorApi()

// Mount infoview
const infoviewApi = renderInfoview(editorApi, document.getElementById('root')!)

let initialized = false

// Forward bridge SSE events → InfoviewApi
editorApi.onServerNotification(async (method, params) => {
  if (method === 'lsp:ready') {
    // LSP server initialized — kick off the infoview
    await infoviewApi.serverRestarted(params as InitializeResult)
    if (!initialized) {
      initialized = true
      await infoviewApi.initialize({
        uri: '',
        range: { start: { line: 0, character: 0 }, end: { line: 0, character: 0 } },
      })
    }
  } else {
    await infoviewApi.gotServerNotification(method, params).catch(() => {})
  }
})

// Expose updateCursor for Emacs → xwidget-webkit-execute-script
;(window as unknown as Record<string, unknown>).updateCursor = async (
  uri: string,
  line: number,
  character: number,
) => {
  if (!initialized) return
  await infoviewApi.changedCursorLocation({
    uri,
    range: { start: { line, character }, end: { line, character } },
  }).catch(() => {})
}

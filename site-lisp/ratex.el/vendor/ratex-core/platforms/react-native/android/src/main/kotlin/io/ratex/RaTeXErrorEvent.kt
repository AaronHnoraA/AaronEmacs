// RaTeXErrorEvent.kt — Fabric event for RaTeXView onError.

package io.ratex

import com.facebook.react.bridge.Arguments
import com.facebook.react.uimanager.events.Event

class RaTeXErrorEvent(
    surfaceId: Int,
    viewId: Int,
    private val errorMessage: String,
) : Event<RaTeXErrorEvent>(surfaceId, viewId) {

    override fun getEventName(): String = EVENT_NAME

    override fun getEventData() = Arguments.createMap().apply {
        putString("error", errorMessage)
    }

    companion object {
        const val EVENT_NAME = "topError"
    }
}

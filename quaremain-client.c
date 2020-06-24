/*
 * Copyright (C) 2020, Momozor
 * Copyright (C) 2006, 2007 Apple Inc.
 * Copyright (C) 2007 Alp Toker <alp@atoker.com>
 * Copyright (C) 2011 Lukasz Slachciak
 * Copyright (C) 2011 Bob Murphy
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY APPLE COMPUTER, INC. ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL APPLE COMPUTER, INC. OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <gtk/gtk.h>
#include <webkit2/webkit2.h>


static void
destroyWindowCallback();

static gboolean
closeWebViewCallback(WebKitWebView* webView, GtkWidget* window);


static void
destroyWindowCallback() {
  gtk_main_quit();
}

static gboolean
closeWebViewCallback(
                     __attribute__((unused))WebKitWebView* webView,
                     GtkWidget* window
                     ) {
  gtk_widget_destroy(window);
  return TRUE;
}


#define DEFAULT_HOST "http://127.0.0.1"
#define DEFAULT_PORT "5000"

int main(int argc, char* argv[]) {
  gtk_init(&argc, &argv);

  GtkWidget *main_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(main_window), "Quaremain");
  gtk_window_set_default_size(GTK_WINDOW(main_window), 1280, 720);

  WebKitWebContext *context = webkit_web_context_new();
  webkit_web_context_set_cache_model(context, WEBKIT_CACHE_MODEL_DOCUMENT_VIEWER);
    
  WebKitWebView *webView = WEBKIT_WEB_VIEW(webkit_web_view_new_with_context(context));

  gtk_container_add(GTK_CONTAINER(main_window), GTK_WIDGET(webView));

  g_signal_connect(main_window, "destroy", G_CALLBACK(destroyWindowCallback), NULL);
  g_signal_connect(webView, "close", G_CALLBACK(closeWebViewCallback), main_window);

  char address_buffer[80];
  char* port = getenv("QUAREMAIN_PORT");

  sprintf(address_buffer, "%s:%s/experimental", DEFAULT_HOST, DEFAULT_PORT);
  if (port != NULL) {
    sprintf(address_buffer, "%s:%s/experimental", DEFAULT_HOST, port);
  }
  
  // Load a web page into the browser instance
  webkit_web_view_load_uri(webView, address_buffer);

  // Make sure that when the browser area becomes visible, it will get mouse
  // and keyboard events
  gtk_widget_grab_focus(GTK_WIDGET(webView));

  // Make sure the main window and all its contents are visible
  gtk_widget_show_all(main_window);

  gtk_main();

  return 0;
}

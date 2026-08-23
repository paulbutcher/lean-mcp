/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import MCP.Protocol
public import MCP.Types
public import MCP.JsonRpc
public import MCP.Server
public import MCP.Base64
public import MCP.Dispatch
public import MCP.Http
public import MCP.Auth
public import MCP.StdHttp

/-!
An MCP server: JSON-RPC 2.0 over the Streamable HTTP transport, tools and nothing else, and both
protocol eras from one set of types.

The application owns its tools and decides who is asking. `MCP.serve` owns the protocol: it
reads the parts of an HTTP request that matter, works out which revision the client is speaking,
and answers. Nothing here is `partial`, nothing panics, and no request leaves a trace that the
next one could read.
-/

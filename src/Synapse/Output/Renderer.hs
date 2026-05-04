-- | Stream-item renderer dispatch (OUTPUT-2).
--
-- This module establishes the renderer dispatch hook for synapse's
-- @--json@ / @--raw@ / template / shaped-JSON / (future) table output paths.
-- It deliberately knows nothing about the concrete renderers — those live
-- in their own modules and produce a 'StreamRenderer' value via a builder.
--
-- == Design
--
-- A 'StreamRenderer' is a tiny state machine over the stream:
--
-- * 'rrOnItem' is invoked once per 'HubStreamItem' as it arrives.
-- * 'rrOnEnd' is invoked once after the stream completes (e.g. so that
--   aggregating renderers like @--count@ or @--tail@ can emit their
--   final output).
--
-- Drivers (i.e. the streaming entry points in @Main.hs@) call
-- 'feedItem' for each item and 'finalize' once at the end. Renderers
-- carry their own state via closures over 'IORef's; the driver doesn't
-- need to know.
--
-- == Extensibility
--
-- New output formats plug in by adding a new module that exports a
-- builder of type @opts -> IO StreamRenderer@. None of the code in this
-- module nor in any other renderer module needs to change.
--
-- This is the contract OUTPUT-3 (table renderer) plugs into.
module Synapse.Output.Renderer
  ( StreamRenderer(..)
  , feedItem
  , finalize
  , passthroughRenderer
  ) where

import Synapse.Schema.Types (HubStreamItem)

-- | A renderer is a small state machine: handle each item, then finalize.
--
-- Renderers carry their own state via closures (typically over an
-- 'Data.IORef.IORef'). The driver only needs the two callbacks here.
data StreamRenderer = StreamRenderer
  { rrOnItem :: HubStreamItem -> IO ()
    -- ^ Called once per stream item as it arrives.
  , rrOnEnd  :: IO ()
    -- ^ Called once after the stream completes. Aggregating renderers
    -- (e.g. @--count@, @--tail@, table) emit their final output here.
    -- For pass-through renderers this is 'pure ()'.
  }

-- | Drive one stream item through the renderer.
feedItem :: StreamRenderer -> HubStreamItem -> IO ()
feedItem = rrOnItem

-- | Finalize the renderer after the stream completes.
finalize :: StreamRenderer -> IO ()
finalize = rrOnEnd

-- | A trivial renderer that does nothing. Useful as a base for renderers
-- whose 'rrOnEnd' is also a no-op.
passthroughRenderer :: (HubStreamItem -> IO ()) -> StreamRenderer
passthroughRenderer onItem = StreamRenderer
  { rrOnItem = onItem
  , rrOnEnd  = pure ()
  }

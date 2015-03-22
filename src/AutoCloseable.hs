module AutoCloseable where


class AutoCloseable a where
    tryWith :: (a -> IO b) -> IO b

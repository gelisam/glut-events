glut-events
===========

For writing GLUT's main loop in the style of Haskell's main function.

(in development)


purpose
-------

Haskell's [Graphics.UI.GLUT](http://cvs.haskell.org/Hugs/pages/libraries/GLUT/Graphics-UI-GLUT.html) library exposes [GLUT](http://www.opengl.org/resources/libraries/glut/)'s approach to event handling: register global callbacks for the Keyboard, Mouse, Timer, Resizing, Redrawing, and Idle events. This encourages a style of development focussing on a shared global state, a style which is not typical in Haskell.

The purpose of this library will be to offer a Haskell-style wrapper around GLUT's platform-independent Input and Timing routines. In particular, much like Haskell's main method is typically written as an infinite tree of alternative IO sequences:

    main = do putStrLn "are you sure? (y/n)"
              s <- getLine
              case s of
                "y" -> putStrLn "full steam ahead!"
                "n" -> putStrLn "ok then."
                _ -> do putStrLn "please answer with 'y' or 'n'."
                        main

The goal is to allow you to write GLUT programs as an infinite tree of alternatives in some yet-to-be-determined monad:

    glutMessage = do clear [ColorBuffer]
                     renderString StrokeFont "are you sure? (y/n)"
                     swapBuffers
    
    glut-main = do lift $ glutMessage "are you sure? (y/n)"
                   Char c <- getKeypress
                   case c of
                     'y' -> lift $ glutMessage "full steam ahead!"
                     'n' -> lift $ glutMessage "ok then."
                     _ -> do lift $ glutMessage "please answer with 'y' or 'n'."
                             waitForTimeout 1000
                             glut-main


long term goal
--------------

The above doesn't look to hard to implement. The hard part would be to allow waiting on multiple events (C-style select()), forking cooperative threads, sending events between threads, and releasing GLUT's idle callback when no threads are waiting for input. I still have no idea how to do this in a typesafe way, though, so don't hold your breath!

-- | forword n steps and rotate n radian infinitely
foreverSpiral :: Double -> Double -> Program
foreverSpiral steps redian = 
            forward steps
            right redian
            foreverSpiral steps+2 redian
-- | A menu is a collection of labeled wizards. Construct with `choice` and compose with `mappend` @(<>)@ in the 'Monoid' instance
newtype Menu b a = MenuC [(String, Wizard b a)]
   deriving (Monoid)


-- | Constructor for a menu consisting of a single option
choice :: PromptString -> Wizard b a -> Menu b a
choice s w = MenuC [(s,w)]

-- | Run a menu with the given prompt string.
-- menu :: PromptString -> Menu b a -> Wizard b a
-- menu s (MenuC l) = join $ join $ fmap liftMaybe $ prompt $ Menu s l


import Test.Tasty
import qualified Test.Queue as Queue
main :: IO ()
main = defaultMain $ testGroup "all tests" [
        testGroup "Queues" Queue.allQueueTests
    ]

### Compile modules

```
> c(time).
> c(logger).
> c(worker).
> c(test).
```

### Run tests

With Jitter = 0, messages should be logged in the right order

``> test:run(1,0).``

With Jitter = 1, some messages will be logged in the wrong order, as we are introducing a delay between sending message to Peer and sending it to Logger. The Peer on the other hand will log it as soon as it is received.

``> test:run(1,1).``

### Notes

Sleep time defines the frequency in which Workers send messages to each other

Jitter time represents the delay between sending a message to a Peer and logging it

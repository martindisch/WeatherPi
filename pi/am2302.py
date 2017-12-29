import sys
import Adafruit_DHT


def read(pin):
    """Attempt reading temperature and humidity on the given pin.

    Parameters
    ----------
    pin : str
        The Raspberry Pi's GPIO data pin to read from

    Returns
    -------
    str
        A string with temperature and humidity separated by a comma or
        "failure" if data could not be read
    """
    hum, temp = Adafruit_DHT.read_retry(Adafruit_DHT.AM2302, pin)
    if hum is not None and temp is not None:
        print("{0:0.1f},{1:0.1f}".format(temp, hum))
    else:
        print("failure")
        sys.exit(1)


if __name__ == '__main__':
    if len(sys.argv) == 1:
        print("Usage: python am2302.py <pin>")
        sys.exit(1)

    read(sys.argv[1])

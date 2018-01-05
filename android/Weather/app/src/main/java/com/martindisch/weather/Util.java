package com.martindisch.weather;

import android.util.JsonReader;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;

/**
 * Contains utility functions for result parsing.
 */
public class Util {

    /**
     * Reads the server's JSON response as a byte array and returns a string array containing
     * the time, temperature and humidity.
     *
     * @param responseBody the server's JSON response as a byte array
     * @return a string array containing the time, temperature and humidity
     * @throws IOException if an error occurred during JSON parsing
     */
    public static String[] parseEntry(byte[] responseBody) throws IOException {
        JsonReader reader = new JsonReader(new InputStreamReader(new ByteArrayInputStream(responseBody)));
        reader.beginArray();
        String[] result = new String[3];
        result[0] = reader.nextString();
        result[1] = reader.nextString();
        result[2] = reader.nextString();
        reader.endArray();
        reader.close();
        return result;
    }

    /**
     * Reads the server's JSON response as a byte array and returns an ArrayList of string arrays,
     * each containing the time, temperature and humidity of a certain point in time.
     *
     * @param responseBody the server's JSON response as a byte array
     * @return an ArrayList containing string arrays with the time, temperature and humidity of a
     * measurement
     * @throws IOException if an error occurred during JSON parsing
     */
    public static ArrayList<String[]> parseHistory(byte[] responseBody) throws IOException {
        JsonReader reader = new JsonReader(new InputStreamReader(new ByteArrayInputStream(responseBody)));
        ArrayList<String[]> results = new ArrayList<>();
        reader.beginArray();
        while (reader.hasNext()) {
            reader.beginArray();
            String[] current = new String[3];
            current[0] = reader.nextString();
            current[1] = reader.nextString();
            current[2] = reader.nextString();
            reader.endArray();
            results.add(current);
        }
        reader.endArray();
        reader.close();
        return results;
    }

    /**
     * Takes a time of the form "2017/01/13 14:39:37" and returns a time
     * of the form "13.01. 14:39".
     *
     * @param longTime the original time, e.g. "2017/01/13 14:39:37"
     * @return the newly formatted time "13.01. 14:39"
     */
    public static String shortenTime(String longTime) {
        return longTime.substring(8, 10) + "." +
                longTime.substring(5, 7) + ". " +
                longTime.substring(11, 16);
    }

}

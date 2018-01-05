package com.martindisch.weather;

import android.util.JsonReader;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Locale;
import java.util.TimeZone;

/**
 * Contains utility functions for result parsing.
 */
public class Util {

    /**
     * Reads the server's JSON response as a byte array and returns a string array containing
     * the time, temperature and humidity.
     *
     * @param responseBody the server's JSON response as a byte array
     * @return a Number array containing the time (long), temperature (double) and humidity (double)
     * @throws IOException if an error occurred during JSON parsing
     */
    public static Number[] parseEntry(byte[] responseBody) throws IOException {
        JsonReader reader = new JsonReader(new InputStreamReader(new ByteArrayInputStream(responseBody)));
        reader.beginArray();
        Number[] result = new Number[3];
        result[0] = reader.nextLong();
        result[1] = reader.nextDouble();
        result[2] = reader.nextDouble();
        reader.endArray();
        reader.close();
        return result;
    }

    /**
     * Reads the server's JSON response as a byte array and returns an ArrayList of string arrays,
     * each containing the time, temperature and humidity of a certain point in time.
     *
     * @param responseBody the server's JSON response as a byte array
     * @return an ArrayList containing Number arrays with the time (long), temperature (double)
     * and humidity (double) of a measurement
     * @throws IOException if an error occurred during JSON parsing
     */
    public static ArrayList<Number[]> parseHistory(byte[] responseBody) throws IOException {
        JsonReader reader = new JsonReader(new InputStreamReader(new ByteArrayInputStream(responseBody)));
        ArrayList<Number[]> results = new ArrayList<>();
        reader.beginArray();
        while (reader.hasNext()) {
            reader.beginArray();
            Number[] current = new Number[3];
            current[0] = reader.nextLong();
            current[1] = reader.nextDouble();
            current[2] = reader.nextDouble();
            reader.endArray();
            results.add(current);
        }
        reader.endArray();
        reader.close();
        return results;
    }

    /**
     * Takes a UNIX timestamp in UTC and returns the corresponding local time
     * of the form "05.01. 11:29".
     *
     * @param secondsUTC the original time, e.g. 1515148190
     * @return the newly formatted time "05.01. 11:29"
     */
    public static String shortTime(long secondsUTC) {
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
        cal.setTimeInMillis(secondsUTC * 1000L);
        SimpleDateFormat formatter = new SimpleDateFormat("dd.MM HH:mm", Locale.getDefault());
        return formatter.format(cal.getTime());
    }

}

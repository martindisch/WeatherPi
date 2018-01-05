package com.martindisch.weather;

import android.graphics.Color;
import android.os.Bundle;
import android.support.v4.content.ContextCompat;
import android.support.v4.widget.SwipeRefreshLayout;
import android.support.v7.app.AppCompatActivity;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.RadioGroup;
import android.widget.TextView;
import android.widget.Toast;

import com.github.mikephil.charting.charts.LineChart;
import com.github.mikephil.charting.components.AxisBase;
import com.github.mikephil.charting.components.XAxis;
import com.github.mikephil.charting.data.Entry;
import com.github.mikephil.charting.data.LineData;
import com.github.mikephil.charting.data.LineDataSet;
import com.github.mikephil.charting.formatter.IAxisValueFormatter;
import com.loopj.android.http.AsyncHttpClient;
import com.loopj.android.http.AsyncHttpResponseHandler;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import cz.msebera.android.httpclient.Header;

public class MainActivity extends AppCompatActivity implements View.OnClickListener {

    private TextView mLatestTemp, mLatestHum;
    private SwipeRefreshLayout mSwipeContainer;
    private LineChart mChart;
    private RadioGroup mTimeframe;
    private Button mLoadGraph;
    private byte[] mLastResponse = null;
    private int mTimeFrameSelection = ALL;

    private static final int ALL = 0, WEEK = 1, DAY = 2;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        mLatestTemp = (TextView) findViewById(R.id.tvLatestTemp);
        mLatestHum = (TextView) findViewById(R.id.tvLatestHum);
        mSwipeContainer = (SwipeRefreshLayout) findViewById(R.id.swipeContainer);
        mChart = (LineChart) findViewById(R.id.chart);
        mLoadGraph = (Button) findViewById(R.id.bLoadGraph);
        mLoadGraph.setOnClickListener(this);

        mTimeframe = (RadioGroup) findViewById(R.id.rgTimeframe);
        mTimeframe.check(R.id.rbAll);
        mTimeframe.setOnCheckedChangeListener(new RadioGroup.OnCheckedChangeListener() {
            @Override
            public void onCheckedChanged(RadioGroup radioGroup, int i) {
                switch (i) {
                    case R.id.rbAll:
                        mTimeFrameSelection = ALL;
                        break;
                    case R.id.rbWeek:
                        mTimeFrameSelection = WEEK;
                        break;
                    case R.id.rbDay:
                        mTimeFrameSelection = DAY;
                        break;
                }
                if (mLastResponse != null) updateAll(mLastResponse, mTimeFrameSelection);
            }
        });

        mChart.setDescription(null);
        mChart.setHighlightPerDragEnabled(false);
        mChart.setHighlightPerTapEnabled(false);
        mChart.setPinchZoom(true);
        mChart.getLegend().setDrawInside(true);
        mChart.setExtraTopOffset(10);
        XAxis xAxis = mChart.getXAxis();
        xAxis.setLabelRotationAngle(-90);
        xAxis.setLabelCount(10);

        mSwipeContainer.setColorSchemeColors(ContextCompat.getColor(this, R.color.colorAccent));
        mSwipeContainer.setEnabled(false);
    }

    @Override
    protected void onResume() {
        super.onResume();
        fetchAndUpdate(false);
    }

    /**
     * Gets data from the server (either whole history or only latest data depending on parameter
     * and calls the respective update method to display it.
     *
     * @param all whether to get whole history (true) or only latest data (false)
     */
    private void fetchAndUpdate(final boolean all) {
        mSwipeContainer.setRefreshing(true);
        AsyncHttpClient client = new AsyncHttpClient();
        client.setMaxRetriesAndTimeout(1, 500);
        client.get("http://" + getString(R.string.IP) + (all ? "/history" : "/latest"), new AsyncHttpResponseHandler() {
            @Override
            public void onSuccess(int statusCode, Header[] headers, byte[] responseBody) {
                if (all) {
                    mLastResponse = responseBody;
                    updateAll(responseBody, mTimeFrameSelection);
                } else {
                    updateLatest(responseBody);
                }
            }

            @Override
            public void onFailure(int statusCode, Header[] headers, byte[] responseBody, Throwable error) {
                Toast.makeText(getApplicationContext(), getString(R.string.error_connecting), Toast.LENGTH_SHORT).show();
                mSwipeContainer.setRefreshing(false);
            }
        });
    }

    /**
     * Receives the server's response (full history), parses it, and displays the latest data as
     * well as the graph.
     *
     * @param responseBody the server's response
     * @param timeframe    the timeframe to show in the graph, one of<br/>
     *                     ALL = 0<br/>
     *                     WEEK = 1<br/>
     *                     DAY = 2
     */
    private void updateAll(final byte[] responseBody, final int timeframe) {
        new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    ArrayList<String[]> history = Util.parseHistory(responseBody);
                    List<String[]> cutHistory = history;
                    if (timeframe == WEEK && history.size() >= 10080) {
                        cutHistory = history.subList(history.size() - 10080, history.size());
                    } else if (timeframe == DAY && history.size() >= 1440) {
                        cutHistory = history.subList(history.size() - 1440, history.size());
                    }
                    final List<String[]> fCutHistory = cutHistory;
                    final String[] latest = history.get(history.size() - 1);
                    ArrayList<Entry> temperature = new ArrayList<>(fCutHistory.size());
                    ArrayList<Entry> humidity = new ArrayList<>(fCutHistory.size());

                    float counter = 0;
                    for (String[] current : fCutHistory) {
                        temperature.add(new Entry(counter++, Float.parseFloat(current[1])));
                        humidity.add(new Entry(counter, Float.parseFloat(current[2])));
                    }

                    LineDataSet tempSet = new LineDataSet(temperature, getString(R.string.temperature));
                    tempSet.setDrawCircles(false);
                    tempSet.setColor(Color.RED);
                    LineDataSet humSet = new LineDataSet(humidity, getString(R.string.humidity));
                    humSet.setDrawCircles(false);
                    humSet.setColor(Color.BLUE);

                    final LineData lineData = new LineData(tempSet, humSet);
                    runOnUiThread(new Runnable() {
                        @Override
                        public void run() {
                            mLatestTemp.setText(String.format(getString(R.string.format_temp), latest[1]));
                            mLatestHum.setText(String.format(getString(R.string.format_hum), latest[2]));
                            mChart.fitScreen();

                            IAxisValueFormatter formatter = new IAxisValueFormatter() {
                                @Override
                                public String getFormattedValue(float value, AxisBase axis) {
                                    int xValue = value >= fCutHistory.size() ? fCutHistory.size() - 1 : (int) value;
                                    return Util.shortenTime(fCutHistory.get(xValue)[0]);
                                }
                            };
                            mChart.getXAxis().setValueFormatter(formatter);
                            mChart.setData(lineData);
                            mChart.invalidate();
                        }
                    });
                } catch (IOException e) {
                    runOnUiThread(new Runnable() {
                        @Override
                        public void run() {
                            Toast.makeText(getApplicationContext(), getString(R.string.error_parsing), Toast.LENGTH_SHORT).show();
                        }
                    });
                } finally {
                    runOnUiThread(new Runnable() {
                        @Override
                        public void run() {
                            mSwipeContainer.setRefreshing(false);
                        }
                    });
                }
            }
        }).start();
    }

    /**
     * Receives the server's response (latest data), parses and displays it.
     *
     * @param responseBody the server's response
     */
    private void updateLatest(final byte[] responseBody) {
        try {
            String[] latest = Util.parseEntry(responseBody);
            mLatestTemp.setText(String.format(getString(R.string.format_temp), latest[1]));
            mLatestHum.setText(String.format(getString(R.string.format_hum), latest[2]));
        } catch (IOException e) {
            Toast.makeText(getApplicationContext(), getString(R.string.error_parsing), Toast.LENGTH_SHORT).show();
        }
        mSwipeContainer.setRefreshing(false);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.action_refresh:
                fetchAndUpdate(false);
                break;
        }
        return super.onOptionsItemSelected(item);
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.menu_main, menu);
        return super.onCreateOptionsMenu(menu);
    }

    @Override
    public void onClick(View view) {
        if (view.getId() == R.id.bLoadGraph) {
            fetchAndUpdate(true);
        }
    }
}

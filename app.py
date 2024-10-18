import dash
from dash import dcc, html
from dash.dependencies import Input, Output
import plotly.graph_objs as go
import numpy as np
import dash_bootstrap_components as dbc

# Define the application with Bootstrap
app = dash.Dash(__name__, external_stylesheets=[dbc.themes.BOOTSTRAP])

# Layout styling
app.layout = html.Div([
    dbc.Container([
        html.H1("Queueing Theory Model for Fog Computing", className="text-center my-4", style={'color': '#4b8bbe'}),

        dbc.Row([
            dbc.Col([
                dbc.Card([
                    dbc.CardHeader("Input Parameters"),
                    dbc.CardBody([
                        dbc.CardGroup([
                            dbc.Label("SLA: Guaranteed response time (ms)"),
                            dcc.Input(id='sla', type='number', value=4, min=0, max=10, step=1, className='form-control'),
                        ]),
                        dbc.CardGroup([
                            dbc.Label("J: # Jobs (Customers)"),
                            dcc.Input(id='jobs', type='number', value=100, min=0, max=500, step=10, className='form-control'),
                        ]),
                        dbc.CardGroup([
                            dbc.Label("μ^E: Service rate (ES)"),
                            dcc.Input(id='mu_es', type='number', value=0.9, min=0.1, max=1, step=0.01, className='form-control'),
                        ]),
                        dbc.CardGroup([
                            dbc.Label("μ^D: Service rate (DS)"),
                            dcc.Input(id='mu_ds', type='number', value=0.4, min=0.1, max=1, step=0.01, className='form-control'),
                        ]),
                        dbc.CardGroup([
                            dbc.Label("μ^P: Service rate (PS)"),
                            dcc.Input(id='mu_ps', type='number', value=0.4, min=0.1, max=1, step=0.01, className='form-control'),
                        ]),
                        dbc.CardGroup([
                            dbc.Label("μ^O: Service rate (OS)"),
                            dcc.Input(id='mu_os', type='number', value=0.4, min=0.1, max=1, step=0.01, className='form-control'),
                        ]),
                        dbc.CardGroup([
                            dbc.Label("μ^F: Service rate (FS)"),
                            dcc.Input(id='mu_fs', type='number', value=0.4, min=0.1, max=1, step=0.01, className='form-control'),
                        ]),
                        dbc.CardGroup([
                            dbc.Label("μ^C: Service rate (CS)"),
                            dcc.Input(id='mu_cs', type='number', value=0.4, min=0.1, max=1, step=0.01, className='form-control'),
                        ]),
                        dbc.CardGroup([
                            dbc.Label("R: # Processing Servers"),
                            dcc.Input(id='m_ps', type='number', value=10, min=1, max=500, step=1, className='form-control'),
                        ]),
                        dbc.CardGroup([
                            dbc.Label("N: # Fog Servers"),
                            dcc.Input(id='m_fs', type='number', value=10, min=1, max=500, step=1, className='form-control'),
                        ]),
                        dbc.CardGroup([
                            dbc.Label("M: # Client Servers"),
                            dcc.Input(id='m_cs', type='number', value=10, min=1, max=500, step=1, className='form-control'),
                        ]),
                        dbc.CardGroup([
        dbc.Card([
            dbc.CardBody([
                dbc.Row([
                    dbc.Col([
                        dbc.Label("δ: Database access probability"),
                        dcc.Slider(
                            id='delta',
                            min=0,
                            max=1,
                            step=0.1,
                            value=0.5,
                            marks={0: '0.0', 0.2: '0.2', 0.3: '0.3', 0.4: '0.4', 0.5: '0.5',
                                   0.6: '0.6', 0.7: '0.7', 0.8: '0.8', 0.9: '0.9', 1: '1.0'},
                            tooltip={"placement": "bottom", "always_visible": True},
                            updatemode='drag',
                        )
                    ])
                ])
            ])
        ])
    ]),
                        dbc.CardGroup([
        dbc.Card([
            dbc.CardBody([
                dbc.Row([
                    dbc.Col([
                        dbc.Label("τ: Output server Probability"),
                        dcc.Slider(
                            id='tau',
                            min=0,
                            max=1,
                            step=0.1,
                            value=0.5,
                            marks={0: '0.0', 0.2: '0.2', 0.3: '0.3', 0.4: '0.4', 0.5: '0.5',
                                   0.6: '0.6', 0.7: '0.7', 0.8: '0.8', 0.9: '0.9', 1: '1.0'},
                            tooltip={"placement": "bottom", "always_visible": True},
                            updatemode='drag',
                        )
                    ])
                ])
            ])
        ])
    ]),
                        dbc.CardGroup([
        dbc.Card([
            dbc.CardBody([
                dbc.Row([
                    dbc.Col([
                        dbc.Label("κ: Fog Server Exit Probability "),
                        dcc.Slider(
                            id='kappa',
                            min=0,
                            max=1,
                            step=0.1,
                            value=0.5,
                            marks={0: '0.0', 0.2: '0.2', 0.3: '0.3', 0.4: '0.4', 0.5: '0.5',
                                   0.6: '0.6', 0.7: '0.7', 0.8: '0.8', 0.9: '0.9', 1: '1.0'},
                            tooltip={"placement": "bottom", "always_visible": True},
                            updatemode='drag',
                        )
                    ])
                ])
            ])
        ])
    ]),
                        dbc.Button('Calculate', id='calc', color='primary', className='mt-3'),
                    ])
                ])
            ], width=6),  # Adjust column width

            dbc.Col([
                dbc.Card([
                    dbc.CardHeader("Results"),
                    dbc.CardBody([
                        dcc.Tabs([

                            dcc.Tab(label='Performance Plots', children=[
                                dcc.Graph(id='throughput-plot'),
                                dcc.Graph(id='response-time-plot'),
                                dcc.Graph(id='mean-customers-plot'),
                                dcc.Graph(id='node-usage-plot')
                            ]),
                            dcc.Tab(label='Summary', children=[
                                html.Pre(id='summary-output', style={'padding': '20px', 'backgroundColor': '#f5f5f5'})
                            ])
                        ])
                    ])
                ])
            ], width=6)  # Adjust column width
        ])
    ], fluid=True)
])

# Callback for updating model and plots
@app.callback(
    [
        Output('throughput-plot', 'figure'),
        Output('response-time-plot', 'figure'),
        Output('mean-customers-plot', 'figure'),
        Output('node-usage-plot', 'figure'),
        Output('summary-output', 'children')
    ],
    [
        Input('sla', 'value'),
        Input('jobs', 'value'),
        Input('mu_es', 'value'),
        Input('mu_ds', 'value'),
        Input('mu_ps', 'value'),
        Input('mu_os', 'value'),
        Input('mu_fs', 'value'),
        Input('mu_cs', 'value'),
        Input('m_ps', 'value'),
        Input('m_fs', 'value'),
        Input('m_cs', 'value'),
        Input('delta', 'value'),
        Input('tau', 'value'),
        Input('kappa', 'value'),
        Input('calc', 'n_clicks')
    ]
)
def update_model(sla, jobs, mu_es, mu_ds, mu_ps, mu_os, mu_fs, mu_cs, m_ps, m_fs, m_cs, delta, tau, kappa, n_clicks):
    # Simulate the queueing model and update results
    # (Replace with actual simulation code)

    # For simplicity, create random data for demonstration
    throughput = np.random.normal(loc=0.5, scale=0.1, size=jobs)
    mean_time = np.random.normal(loc=5, scale=2, size=jobs)
    mean_customers = np.random.normal(loc=10, scale=3, size=jobs)
    node_usage = np.random.normal(loc=70, scale=15, size=m_ps)

    # Create plots with axis labels
    throughput_fig = go.Figure(data=go.Scatter(x=np.arange(jobs), y=throughput, mode='lines', name='Throughput'))
    throughput_fig.update_layout(title='Throughput vs. Jobs', xaxis_title='Jobs', yaxis_title='Throughput')

    mean_time_fig = go.Figure(data=go.Scatter(x=np.arange(jobs), y=mean_time, mode='lines', name='Response Time'))
    mean_time_fig.update_layout(title='Response Time vs. Jobs', xaxis_title='Jobs', yaxis_title='Response Time (ms)')

    mean_customers_fig = go.Figure(data=go.Scatter(x=np.arange(jobs), y=mean_customers, mode='lines', name='Mean Customers'))
    mean_customers_fig.update_layout(title='Mean Customers vs. Jobs', xaxis_title='Jobs', yaxis_title='Mean Customers')

    node_usage_fig = go.Figure(data=go.Bar(x=np.arange(m_ps), y=node_usage, name='Node Usage'))
    node_usage_fig.update_layout(title='Node Usage vs. Processing Servers', xaxis_title='Processing Servers', yaxis_title='Usage (%)')

    # Create a summary output
    summary_text = f"Summary:\nSLA: {sla} ms\nTotal Jobs: {jobs}\nProcessing Servers: {m_ps}\nFog Servers: {m_fs}\nClient Servers: {m_cs}\n\nThroughput: {np.mean(throughput):.2f}\nMean Response Time: {np.mean(mean_time):.2f} ms\nMean Customers: {np.mean(mean_customers):.2f}\nNode Usage: {np.mean(node_usage):.2f}%"

    return throughput_fig, mean_time_fig, mean_customers_fig, node_usage_fig, summary_text


if __name__ == '__main__':
    app.run_server(debug=True)

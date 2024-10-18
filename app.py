import dash
from dash import dcc, html
from dash.dependencies import Input, Output
import plotly.graph_objs as go
import numpy as np
import pandas as pd

# Define application
app = dash.Dash(__name__)

app.layout = html.Div([
    html.H1("Queueing Theory Model for Fog Computing"),
    
    html.Div([
        html.Label("SLA: Guaranteed response time"),
        dcc.Input(id='sla', type='number', value=4, min=0, max=10, step=1),
    ]),

    html.Div([
        html.Label("J: # Jobs (Customers)"),
        dcc.Input(id='jobs', type='number', value=100, min=0, max=500, step=10),
    ]),

    html.Div([
        html.Label("μ^E: Service rate (ES)"),
        dcc.Input(id='mu_es', type='number', value=9/10, min=0.1, max=1, step=0.01),
    ]),

    html.Div([
        html.Label("μ^D: Service rate (DS)"),
        dcc.Input(id='mu_ds', type='number', value=4/10, min=0.1, max=1, step=0.01),
    ]),

    html.Div([
        html.Label("μ^P: Service rate (PS)"),
        dcc.Input(id='mu_ps', type='number', value=4/10, min=0.1, max=1, step=0.01),
    ]),

    html.Div([
        html.Label("μ^O: Service rate (OS)"),
        dcc.Input(id='mu_os', type='number', value=4/10, min=0.1, max=1, step=0.01),
    ]),

    html.Div([
        html.Label("μ^F: Service rate (FS)"),
        dcc.Input(id='mu_fs', type='number', value=4/10, min=0.1, max=1, step=0.01),
    ]),

    html.Div([
        html.Label("μ^C: Service rate (CS)"),
        dcc.Input(id='mu_cs', type='number', value=4/10, min=0.1, max=1, step=0.01),
    ]),

    html.Div([
        html.Label("R: # Processing Servers"),
        dcc.Input(id='m_ps', type='number', value=10, min=1, max=500, step=1),
    ]),

    html.Div([
        html.Label("N: # Fog Servers"),
        dcc.Input(id='m_fs', type='number', value=10, min=1, max=500, step=1),
    ]),

    html.Div([
        html.Label("M: # Client Servers"),
        dcc.Input(id='m_cs', type='number', value=10, min=1, max=500, step=1),
    ]),

    html.Div([
        html.Label("δ: Database access probability"),
        dcc.Slider(id='delta', min=0, max=1, step=0.1, value=0.5),
    ]),

    html.Div([
        html.Label("τ: Output server probability"),
        dcc.Slider(id='tau', min=0, max=1, step=0.1, value=0.5),
    ]),

    html.Div([
        html.Label("κ: Fog server exit probability"),
        dcc.Slider(id='kappa', min=0, max=1, step=0.1, value=0.5),
    ]),

    html.Button('Calculate', id='calc'),

    dcc.Tabs([
        dcc.Tab(label='Performance Plots', children=[
            dcc.Graph(id='throughput-plot'),
            dcc.Graph(id='response-time-plot'),
            dcc.Graph(id='mean-customers-plot'),
            dcc.Graph(id='node-usage-plot')
        ]),
        dcc.Tab(label='Summary', children=[
            html.Pre(id='summary-output')
        ])
    ])
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
    mean_customers = np.random.randint(1, 10, size=6)
    node_usage = np.random.rand(6)

    # Throughput plot
    throughput_fig = go.Figure(data=[go.Scatter(x=np.arange(1, jobs+1), y=throughput, mode='lines+markers')])
    throughput_fig.update_layout(title='Throughput evolution', xaxis_title='Jobs', yaxis_title='Throughput')

    # Response time plot
    response_time_fig = go.Figure(data=[go.Scatter(x=np.arange(1, jobs+1), y=mean_time, mode='lines+markers')])
    response_time_fig.add_shape(
        type="line", x0=1, y0=sla, x1=jobs, y1=sla,
        line=dict(color="Red", width=2, dash="dash")
    )
    response_time_fig.update_layout(title='Mean Time Evolution', xaxis_title='Jobs', yaxis_title='Mean Time')

    # Mean customers plot
    mean_customers_fig = go.Figure(data=[go.Bar(x=['ES', 'PS', 'DS', 'OS', 'CS', 'FS'], y=mean_customers)])
    mean_customers_fig.update_layout(title='Mean Customers in Each Node', xaxis_title='Node', yaxis_title='Mean Jobs')

    # Node usage plot
    node_usage_fig = go.Figure(data=[go.Bar(x=['ES', 'PS', 'DS', 'OS', 'CS', 'FS'], y=node_usage)])
    node_usage_fig.update_layout(title='Node Usage', xaxis_title='Node', yaxis_title='Usage')

    # Summary output
    summary_output = f"Summary: SLA={sla}, Jobs={jobs}"

    return throughput_fig, response_time_fig, mean_customers_fig, node_usage_fig, summary_output

if __name__ == '__main__':
    app.run_server(debug=True)

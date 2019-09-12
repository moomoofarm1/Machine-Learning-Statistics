# @Author ZhuoJun Gu, distributed under GNU license GPL v3.0
# http://www.gnu.org/licenses/gpl-3.0.html

#pragma once

#ifndef __MODULE_ONLINE_ELM
#define __MODULE_ONLINE_ELM

#include <iostream>
#include <vector>
#include <Eigen/Core>


class Online_ELM
{
    private:

	    // model para
	    int window_width;
	    int input_width;
	    int l1_width;
	    MatrixXd l1_weights;
	    MatrixXd l1_bias;
	    MatrixXd Hk;
	    MatrixXd l2_weights_k;
	    int output_width = 1;
		int nof_input_data = 1;

	    // initialize para
		int nof_initial_data;
		int nof_initial_data_counter;
	    MatrixXd initial_data_X;
	    MatrixXd initial_data_Y;

     public:
		
	    void initial_1_model_set(int window_length,int initial_data_size)
	    {
		    // model para
		    window_width = window_length;
		    input_width = window_width - 1;
		    l1_width = input_width * 2 + 1;
		    l1_weights = MatrixXd::Random(l1_width,input_width);
		    l1_bias = MatrixXd::Random(l1_width, output_width);
		    Hk = MatrixXd::Zero(nof_input_data,l1_width);
		    l2_weights_k = MatrixXd::Zero(l1_width, output_width);

		    // initial data para
		    nof_initial_data = initial_data_size;
		    nof_initial_data_counter = -1;
		    initial_data_X = MatrixXd::Zero(input_width,nof_initial_data);
		    initial_data_Y = MatrixXd::Zero(nof_initial_data, output_width);
	    }

	    int initial_1_save_data(vector<double> window)  
	    {
			
			if (nof_initial_data_counter > nof_initial_data - 2)
		    {
				return 0;
			}
		    else
		    {
				nof_initial_data_counter += 1;
				initial_data_X.col(nof_initial_data_counter) = Map<VectorXd>(&window[0], window.size() - 1);
			    initial_data_Y.row(nof_initial_data_counter) = Map<VectorXd>(&window[window.size() - 1], output_width);

				return 0;
		    }
	    }

	    int initial_2_train()
	    {
		    if (nof_initial_data_counter == nof_initial_data - 1)
		    {
				MatrixXd l1_bias_1 = l1_bias * MatrixXd::Ones(output_width, nof_initial_data);
				MatrixXd H0_T = l1_weights * initial_data_X + l1_bias_1; 
				MatrixXd H0 = H0_T.transpose(); 
				MatrixXd K0_inverse = H0_T * H0; 
				K0_inverse = K0_inverse.array().cwiseInverse(); 

				// save online training para
				l2_weights_k = K0_inverse * H0_T * initial_data_Y; 
				l1_bias_1 = l1_bias * MatrixXd::Ones(output_width, nof_input_data); 
				MatrixXd Hk_T = l1_weights * initial_data_X.col(nof_initial_data_counter) + l1_bias_1;  // 此处可按在线输入样本量修改代码，用于准备在线训练的参数
				Hk = Hk_T.transpose(); 

				return 0;
			}
		    else
		    {
				return 0;
		    }
	    }

	    int online_train(vector<double> window)  
	    {
		    if (nof_initial_data_counter == nof_initial_data - 1)
		    {
				// calculate Pk and Hk_1
				MatrixXd Pk = Hk.transpose() * Hk; 
				Pk = Pk.array().cwiseInverse();  
				MatrixXd new_data_X = Map<VectorXd>(&window[0], window.size() - 1);
				MatrixXd new_data_Y = Map<VectorXd>(&window[window.size() - 1], output_width); 
				MatrixXd l1_bias_1 = l1_bias * MatrixXd::Ones(output_width, nof_input_data); 
				MatrixXd Hk_1_T = l1_weights * new_data_X + l1_bias_1; 
				MatrixXd Hk_1 = Hk_1_T.transpose(); 

				// updating result
				MatrixXd inter_data = MatrixXd::Identity(1, 1) + Hk_1 * Pk * Hk_1_T; 
				inter_data = inter_data.array().cwiseInverse(); 
				MatrixXd Pk_1 = Pk - Pk * Hk_1_T * inter_data * Hk_1 * Pk; 
				// update online training para
				l2_weights_k = l2_weights_k + Pk_1 * Hk_1_T * (new_data_Y - Hk_1 * l2_weights_k); 
				Hk = Hk_1;

				return 0;
			}
		    else
		    {
				return 0;
		    }
	    }

	    vector<double> predict(vector<double> window) // 最后一个值为被预测值  BTW：untested
	    {
		    MatrixXd output;
		    MatrixXd new_data_X = Map<VectorXd>(&window[0], window.size() - 1);
		    output = (l1_weights * new_data_X + l1_bias);
		    output = output.transpose() * l2_weights_k;
		    window[window.size() - 1] = output(0, 0);
		    return window;
	    }

	    MatrixXd get_X(void)
	    {
		return initial_data_X;
	    }

		MatrixXd get_Y(void)
		{
			return initial_data_Y;
		}

		MatrixXd get_l2_w(void)
		{
			return l2_weights_k;
		}

		MatrixXd get_l1_w(void)
		{
			return l1_weights;
		}

		MatrixXd get_l1_bias(void)
		{
			return l1_bias;
		}

};

#endif
